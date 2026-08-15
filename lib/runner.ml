(* The measurement loop allocates nothing. A minor collection in OCaml 5 stops
   every domain, and that rendezvous orders memory as surely as a fence would,
   so an allocation in here can quietly hide the reordering the test exists to
   catch. Everything the loop touches is allocated before it starts. *)

(* Spins are bounded. A domain that never arrives ends the run rather than
   hanging it, which is what used to happen when parties outnumbered cores. *)
let spin_limit = 50_000_000

(* An outcome is encoded into a flat table, so this caps radix ^ width. *)
let max_slots = 4096

type barrier = {
  waiting : int Atomic.t;
  sense : bool Atomic.t;
  parties : int;
  aborted : bool Atomic.t;
}

let barrier parties =
  assert (parties > 0);
  {
    waiting = Atomic.make parties;
    sense = Atomic.make false;
    parties;
    aborted = Atomic.make false;
  }

(* Returns false once the run has given up, so callers can wind down. *)
let await b local =
  let target = not !local in
  local := target;
  if Atomic.fetch_and_add b.waiting (-1) = 1 then begin
    Atomic.set b.waiting b.parties;
    Atomic.set b.sense target;
    true
  end
  else begin
    let spun = ref 0 in
    while
      Atomic.get b.sense <> target
      && !spun < spin_limit
      && not (Atomic.get b.aborted)
    do
      Domain.cpu_relax ();
      incr spun
    done;
    if !spun >= spin_limit then Atomic.set b.aborted true;
    not (Atomic.get b.aborted)
  end

type outcome = int array

type 'st spec = {
  name : string;
  doc : string;
  parties : int;
  labels : string array;
  (* Every observed value must land in [0, radix). Anything else is counted
     separately rather than folded into a neighbouring bucket. *)
  radix : int;
  init : unit -> 'st;
  reset : 'st -> unit;
  thread : 'st -> int -> unit;
  (* Fills the caller's buffer. Returning a fresh array would allocate once
     per iteration, which is the thing we are avoiding. *)
  observe : 'st -> outcome -> unit;
  forbidden : outcome -> bool;
  (* A control is built from racy plain accesses, so the forbidden outcome is
     one the hardware may legally produce. It firing proves the harness can
     see a violation at all. *)
  control : bool;
}

type t = Test : 'st spec -> t

type tally = {
  counts : int array;
  mutable out_of_range : int;
  radix : int;
  width : int;
}

(* Zero means the table would be too big, which is a bug in the test rather
   than something to work around at runtime. *)
let slots radix width =
  let n = ref 1 and ok = ref true in
  for _ = 1 to width do
    if !n > max_slots / radix then ok := false else n := !n * radix
  done;
  if !ok then !n else 0

let make_tally radix width =
  let n = slots radix width in
  assert (n > 0);
  { counts = Array.make n 0; out_of_range = 0; radix; width }

(* -1 for anything outside the declared radix. *)
let encode t o =
  let idx = ref 0 and ok = ref true in
  for i = 0 to t.width - 1 do
    let v = o.(i) in
    if v < 0 || v >= t.radix then ok := false else idx := (!idx * t.radix) + v
  done;
  if !ok then !idx else -1

let record t o =
  let i = encode t o in
  if i < 0 then t.out_of_range <- t.out_of_range + 1
  else t.counts.(i) <- t.counts.(i) + 1

let decode t i buf =
  let n = ref i in
  for k = t.width - 1 downto 0 do
    buf.(k) <- !n mod t.radix;
    n := !n / t.radix
  done

type result = {
  test : string;
  iterations : int;
  completed : int;
  seen : (outcome * int) list;
  violations : int;
  out_of_range : int;
  timed_out : bool;
  wall : float;
}

(* Shifts the alignment between domains from one iteration to the next, which
   is what shakes the interesting interleavings out. *)
let jitter seed bound =
  seed := ((!seed * 1103515245) + 12345) land 0x3FFFFFFF;
  for _ = 1 to !seed mod bound do
    Domain.cpu_relax ()
  done

let run ?(jitter_bound = 64) ~iterations (Test spec) =
  let width = Array.length spec.labels in
  assert (spec.parties > 0);
  assert (width > 0);
  assert (spec.radix > 1);
  assert (iterations > 0);
  assert (jitter_bound > 0);
  let t = make_tally spec.radix width in
  let st = spec.init () in
  let buf = Array.make width 0 in
  let start = barrier spec.parties and finish = barrier spec.parties in
  let completed = ref 0 in
  let party i =
    let sense_start = ref false and sense_finish = ref false in
    let seed = ref ((i * 7919) + 1) in
    let n = ref 0 and live = ref true in
    while !live && !n < iterations do
      if i = 0 then spec.reset st;
      live := await start sense_start;
      if !live then begin
        jitter seed jitter_bound;
        spec.thread st i;
        live := await finish sense_finish;
        if !live && i = 0 then begin
          spec.observe st buf;
          record t buf;
          incr completed
        end
      end;
      incr n
    done
  in
  let t0 = Unix.gettimeofday () in
  let others =
    List.init (spec.parties - 1) (fun i ->
        Domain.spawn (fun () -> party (i + 1)))
  in
  party 0;
  List.iter Domain.join others;
  let wall = Unix.gettimeofday () -. t0 in
  let timed_out = Atomic.get start.aborted || Atomic.get finish.aborted in
  let seen = ref [] in
  for i = Array.length t.counts - 1 downto 0 do
    if t.counts.(i) > 0 then begin
      let o = Array.make width 0 in
      decode t i o;
      seen := (o, t.counts.(i)) :: !seen
    end
  done;
  let seen =
    List.sort (fun (_, a) (_, b) -> compare b a) !seen
  in
  let violations =
    List.fold_left
      (fun acc (o, n) -> if spec.forbidden o then acc + n else acc)
      0 seen
  in
  {
    test = spec.name;
    iterations;
    completed = !completed;
    seen;
    violations;
    out_of_range = t.out_of_range;
    timed_out;
    wall;
  }

let show_outcome labels o =
  Array.to_list (Array.mapi (fun i v -> Printf.sprintf "%s=%d" labels.(i) v) o)
  |> String.concat " "

let report (Test spec) r =
  Printf.printf "%s\n" r.test;
  Printf.printf "  %s\n" spec.doc;
  Printf.printf "  %d of %d iterations on %d domains, %.2fs\n" r.completed
    r.iterations spec.parties r.wall;
  if r.timed_out then
    Printf.printf "  TIMED OUT, results below are partial\n";
  if r.out_of_range > 0 then
    Printf.printf "  %d outcomes outside radix %d, not counted\n" r.out_of_range
      spec.radix;
  List.iter
    (fun (o, n) ->
      Printf.printf "    %-30s %10d  %s\n" (show_outcome spec.labels o) n
        (if spec.forbidden o then "FORBIDDEN" else ""))
    r.seen;
  (match (spec.control, r.violations) with
  | false, 0 -> Printf.printf "  ok\n"
  | false, n -> Printf.printf "  VIOLATION: %d forbidden outcomes\n" n
  | true, 0 -> Printf.printf "  control did not fire here\n"
  | true, n -> Printf.printf "  control fired %d times\n" n);
  print_newline ()
