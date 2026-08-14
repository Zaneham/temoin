(* Every shared location gets its own heap block with slack either side. A
   padding field would not do it, since an [int array] field is one pointer
   word and the neighbours stay adjacent. *)

let width = 32
let slot = 16

type shared = {
  data : int array;
  pflag : int array;
  px : int array;
  py : int array;
  flag : bool Atomic.t;
  x : int Atomic.t;
  y : int Atomic.t;
  res : int array;
}

let fresh () = {
  data = Array.make width 0;
  pflag = Array.make width 0;
  px = Array.make width 0;
  py = Array.make width 0;
  flag = Atomic.make_contended false;
  x = Atomic.make_contended 0;
  y = Atomic.make_contended 0;
  res = Array.make width 0;
}

let clear s =
  s.data.(slot) <- 0;
  s.pflag.(slot) <- 0;
  s.px.(slot) <- 0;
  s.py.(slot) <- 0;
  Atomic.set s.flag false;
  Atomic.set s.x 0;
  Atomic.set s.y 0;
  for i = 0 to 5 do s.res.(i) <- 0 done

let mp = Temoin.Test {
  name = "MP (message passing, atomic flag)";
  doc = "T0: data=1; flag:=true   T1: r0=flag; r1=data   forbids r0=1,r1=0";
  parties = 2;
  labels = [| "sawflag"; "data" |];
  init = fresh; reset = clear; control = false;
  thread = (fun s i ->
    if i = 0 then begin
      s.data.(slot) <- 1;
      Atomic.set s.flag true
    end else begin
      s.res.(0) <- (if Atomic.get s.flag then 1 else 0);
      s.res.(1) <- s.data.(slot)
    end);
  observe = (fun s -> [| s.res.(0); s.res.(1) |]);
  forbidden = (fun o -> o.(0) = 1 && o.(1) = 0);
}

(* Not a control. OCaml emits a release fence before plain stores on weakly
   ordered targets, lwsync on power, so the store-store reordering this needs
   cannot happen. A violation here is a real break. *)
let mp_plain = Temoin.Test {
  name = "MP publication (plain accesses)";
  doc = "same shape without atomics; the fence on plain stores should hold it";
  parties = 2;
  labels = [| "sawflag"; "data" |];
  init = fresh; reset = clear; control = false;
  thread = (fun s i ->
    if i = 0 then begin
      s.data.(slot) <- 1;
      s.pflag.(slot) <- 1
    end else begin
      s.res.(0) <- s.pflag.(slot);
      s.res.(1) <- s.data.(slot)
    end);
  observe = (fun s -> [| s.res.(0); s.res.(1) |]);
  forbidden = (fun o -> o.(0) = 1 && o.(1) = 0);
}

let sb = Temoin.Test {
  name = "SB (store buffering, atomic)";
  doc = "T0: x:=1; r0=y   T1: y:=1; r1=x   forbids r0=0,r1=0";
  parties = 2;
  labels = [| "r0"; "r1" |];
  init = fresh; reset = clear; control = false;
  thread = (fun s i ->
    if i = 0 then begin
      Atomic.set s.x 1; s.res.(0) <- Atomic.get s.y
    end else begin
      Atomic.set s.y 1; s.res.(1) <- Atomic.get s.x
    end);
  observe = (fun s -> [| s.res.(0); s.res.(1) |]);
  forbidden = (fun o -> o.(0) = 0 && o.(1) = 0);
}

(* The only genuine control. Store-load is not ordered by the fence OCaml puts
   before plain stores, so nothing prevents this. It fires on amd64 and power. *)
let sb_plain = Temoin.Test {
  name = "SB control (plain accesses)";
  doc = "same without atomics; store-load is unordered by any fence here";
  parties = 2;
  labels = [| "r0"; "r1" |];
  init = fresh; reset = clear; control = true;
  thread = (fun s i ->
    if i = 0 then begin
      s.px.(slot) <- 1; s.res.(0) <- s.py.(slot)
    end else begin
      s.py.(slot) <- 1; s.res.(1) <- s.px.(slot)
    end);
  observe = (fun s -> [| s.res.(0); s.res.(1) |]);
  forbidden = (fun o -> o.(0) = 0 && o.(1) = 0);
}

let lb = Temoin.Test {
  name = "LB (load buffering, atomic)";
  doc = "T0: r0=x; y:=1   T1: r1=y; x:=1   forbids r0=1,r1=1";
  parties = 2;
  labels = [| "r0"; "r1" |];
  init = fresh; reset = clear; control = false;
  thread = (fun s i ->
    if i = 0 then begin
      s.res.(0) <- Atomic.get s.x; Atomic.set s.y 1
    end else begin
      s.res.(1) <- Atomic.get s.y; Atomic.set s.x 1
    end);
  observe = (fun s -> [| s.res.(0); s.res.(1) |]);
  forbidden = (fun o -> o.(0) = 1 && o.(1) = 1);
}

(* Once a domain has seen the new value it must not see the old one again. *)
let corr = Temoin.Test {
  name = "CoRR (read-read coherence)";
  doc = "T0: x:=1   T1: r0=x; r1=x   forbids r0=1,r1=0";
  parties = 2;
  labels = [| "r0"; "r1" |];
  init = fresh; reset = clear; control = false;
  thread = (fun s i ->
    if i = 0 then Atomic.set s.x 1
    else begin
      s.res.(0) <- Atomic.get s.x;
      s.res.(1) <- Atomic.get s.x
    end);
  observe = (fun s -> [| s.res.(0); s.res.(1) |]);
  forbidden = (fun o -> o.(0) = 1 && o.(1) = 0);
}

(* Causality across three domains. If T1 saw T0's write and then published,
   T2 cannot see the publication without the original. *)
let wrc = Temoin.Test {
  name = "WRC (write-to-read causality)";
  doc = "T0: x:=1  T1: r0=x; y:=1  T2: r1=y; r2=x   forbids r0=1,r1=1,r2=0";
  parties = 3;
  labels = [| "r0"; "r1"; "r2" |];
  init = fresh; reset = clear; control = false;
  thread = (fun s i ->
    match i with
    | 0 -> Atomic.set s.x 1
    | 1 ->
        s.res.(0) <- Atomic.get s.x;
        Atomic.set s.y 1
    | _ ->
        s.res.(1) <- Atomic.get s.y;
        s.res.(2) <- Atomic.get s.x);
  observe = (fun s -> [| s.res.(0); s.res.(1); s.res.(2) |]);
  forbidden = (fun o -> o.(0) = 1 && o.(1) = 1 && o.(2) = 0);
}

(* Two writers, two readers. If the readers disagree about which write landed
   first, the writes were not seen in a single order by everyone. *)
let iriw = Temoin.Test {
  name = "IRIW (independent reads, independent writes)";
  doc = "T0: x:=1  T1: y:=1  T2: r0=x,r1=y  T3: r2=y,r3=x   forbids the readers disagreeing";
  parties = 4;
  labels = [| "r0"; "r1"; "r2"; "r3" |];
  init = fresh; reset = clear; control = false;
  thread = (fun s i ->
    match i with
    | 0 -> Atomic.set s.x 1
    | 1 -> Atomic.set s.y 1
    | 2 ->
        s.res.(0) <- Atomic.get s.x;
        s.res.(1) <- Atomic.get s.y
    | _ ->
        s.res.(2) <- Atomic.get s.y;
        s.res.(3) <- Atomic.get s.x);
  observe = (fun s -> [| s.res.(0); s.res.(1); s.res.(2); s.res.(3) |]);
  forbidden = (fun o ->
    o.(0) = 1 && o.(1) = 0 && o.(2) = 1 && o.(3) = 0);
}

let cas = Temoin.Test {
  name = "CAS (mutual exclusion)";
  doc = "both cas 0 -> own tag; exactly one wins and the final value is its tag";
  parties = 2;
  labels = [| "won0"; "won1"; "final" |];
  init = fresh; reset = clear; control = false;
  thread = (fun s i ->
    let won = Atomic.compare_and_set s.x 0 (i + 1) in
    s.res.(i) <- (if won then 1 else 0));
  observe = (fun s -> [| s.res.(0); s.res.(1); Atomic.get s.x |]);
  forbidden = (fun o ->
    (o.(0) = 1 && o.(1) = 1)
    || (o.(0) = 0 && o.(1) = 0)
    || (o.(0) = 1 && o.(2) <> 1)
    || (o.(1) = 1 && o.(2) <> 2));
}

(* Both increment. The two returned values must differ, and the total must
   account for both. *)
let faa = Temoin.Test {
  name = "FAA (fetch and add)";
  doc = "both fetch_and_add 1; the old values must differ and the final must be 2";
  parties = 2;
  labels = [| "got0"; "got1"; "final" |];
  init = fresh; reset = clear; control = false;
  thread = (fun s i -> s.res.(i) <- Atomic.fetch_and_add s.x 1);
  observe = (fun s -> [| s.res.(0); s.res.(1); Atomic.get s.x |]);
  forbidden = (fun o -> o.(0) = o.(1) || o.(2) <> 2);
}

(* Exactly one domain must observe the initial value, and the survivor's tag
   must be what is left behind. *)
let exchange = Temoin.Test {
  name = "XCHG (exchange)";
  doc = "T0 swaps in 1, T1 swaps in 2; exactly one sees 0";
  parties = 2;
  labels = [| "got0"; "got1"; "final" |];
  init = fresh; reset = clear; control = false;
  thread = (fun s i -> s.res.(i) <- Atomic.exchange s.x (i + 1));
  observe = (fun s -> [| s.res.(0); s.res.(1); Atomic.get s.x |]);
  forbidden = (fun o ->
    let saw_initial = (if o.(0) = 0 then 1 else 0) + (if o.(1) = 0 then 1 else 0) in
    saw_initial <> 1
    || (o.(0) = 0 && not (o.(1) = 1 && o.(2) = 2))
    || (o.(1) = 0 && not (o.(0) = 2 && o.(2) = 1)));
}

let all = [ mp; mp_plain; sb; sb_plain; lb; corr; wrc; iriw; cas; faa; exchange ]
