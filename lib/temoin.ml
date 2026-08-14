type barrier = {
  waiting : int Atomic.t;
  sense : bool Atomic.t;
  parties : int;
}

let barrier parties =
  { waiting = Atomic.make parties; sense = Atomic.make false; parties }

(* Spins rather than blocking, so the domains resume within a few hundred
   cycles of each other. *)
let await b local =
  let target = not !local in
  if Atomic.fetch_and_add b.waiting (-1) = 1 then begin
    Atomic.set b.waiting b.parties;
    Atomic.set b.sense target
  end else
    while Atomic.get b.sense <> target do Domain.cpu_relax () done;
  local := target

type outcome = int array

type 'st spec = {
  name : string;
  doc : string;
  parties : int;
  labels : string array;
  init : unit -> 'st;
  reset : 'st -> unit;
  thread : 'st -> int -> unit;
  observe : 'st -> outcome;
  forbidden : outcome -> bool;
  (* A control is built from racy plain accesses, so the forbidden outcome is
     one the hardware may legally produce. It firing proves the harness can
     see a violation at all. *)
  control : bool;
}

type t = Test : 'st spec -> t

type result = {
  test : string;
  iterations : int;
  seen : (outcome * int) list;
  violations : int;
  wall : float;
}

(* Shifts the alignment between domains from one iteration to the next, which
   is what shakes the interesting interleavings out. *)
let jitter seed bound =
  seed := (!seed * 1103515245 + 12345) land 0x3FFFFFFF;
  for _ = 1 to !seed mod bound do Domain.cpu_relax () done

let run ?(jitter_bound = 64) ~iterations (Test spec) =
  let st = spec.init () in
  let start = barrier spec.parties and finish = barrier spec.parties in
  let tally = Hashtbl.create 16 in
  let t0 = Unix.gettimeofday () in
  let party i =
    let sense_start = ref false and sense_finish = ref false in
    let seed = ref (i * 7919 + 1) in
    for _ = 1 to iterations do
      if i = 0 then spec.reset st;
      await start sense_start;
      jitter seed jitter_bound;
      spec.thread st i;
      await finish sense_finish;
      if i = 0 then begin
        let o = spec.observe st in
        Hashtbl.replace tally o
          (1 + Option.value ~default:0 (Hashtbl.find_opt tally o))
      end
    done
  in
  let others =
    List.init (spec.parties - 1) (fun i -> Domain.spawn (fun () -> party (i + 1)))
  in
  party 0;
  List.iter Domain.join others;
  let wall = Unix.gettimeofday () -. t0 in
  let seen =
    Hashtbl.fold (fun o n acc -> (o, n) :: acc) tally []
    |> List.sort (fun (_, a) (_, b) -> compare b a)
  in
  let violations =
    List.fold_left
      (fun acc (o, n) -> if spec.forbidden o then acc + n else acc) 0 seen
  in
  { test = spec.name; iterations; seen; violations; wall }

let show_outcome labels o =
  Array.to_list (Array.mapi (fun i v -> Printf.sprintf "%s=%d" labels.(i) v) o)
  |> String.concat " "

let report (Test spec) r =
  Printf.printf "%s\n" r.test;
  Printf.printf "  %s\n" spec.doc;
  Printf.printf "  %d iterations on %d domains, %.2fs\n"
    r.iterations spec.parties r.wall;
  List.iter
    (fun (o, n) ->
       Printf.printf "    %-30s %10d  %s\n"
         (show_outcome spec.labels o) n
         (if spec.forbidden o then "FORBIDDEN" else ""))
    r.seen;
  (match spec.control, r.violations with
   | false, 0 -> Printf.printf "  ok\n"
   | false, n -> Printf.printf "  VIOLATION: %d forbidden outcomes\n" n
   | true, 0 -> Printf.printf "  control did not fire here\n"
   | true, n -> Printf.printf "  control fired %d times\n" n);
  print_newline ()
