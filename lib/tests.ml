(* Each test owns its state and its layout. One shared record made every
   test's padding everybody else's business, and mixed-size tests will want
   locations deliberately adjacent, which is the opposite of what padding is
   for. *)

let pad = 32
let slot = 16

(* Its own heap block with slack either side. A padding field would not do it,
   since an [int array] field is one pointer word and neighbours stay
   adjacent. *)
let loc () = Array.make pad 0

module Mp = struct
  type st = { data : int array; flag : bool Atomic.t; res : int array }

  let fresh () =
    { data = loc (); flag = Atomic.make_contended false; res = Array.make 2 0 }

  let clear s =
    s.data.(slot) <- 0;
    Atomic.set s.flag false;
    s.res.(0) <- 0;
    s.res.(1) <- 0

  let test =
    Runner.Test
      {
        name = "MP (message passing, atomic flag)";
        doc = "T0: data=1; flag:=true   T1: r0=flag; r1=data   forbids r0=1,r1=0";
        parties = 2;
        labels = [| "sawflag"; "data" |];
        radix = 2;
        init = fresh;
        reset = clear;
        control = false;
        thread =
          (fun s i ->
            if i = 0 then begin
              s.data.(slot) <- 1;
              Atomic.set s.flag true
            end
            else begin
              s.res.(0) <- (if Atomic.get s.flag then 1 else 0);
              s.res.(1) <- s.data.(slot)
            end);
        observe = (fun s o -> o.(0) <- s.res.(0); o.(1) <- s.res.(1));
        forbidden = (fun o -> o.(0) = 1 && o.(1) = 0);
      }
end

(* Not a control. OCaml emits a release fence before plain stores on weakly
   ordered targets, lwsync on power, so the store-store reordering this needs
   cannot happen. A violation here is a real break. *)
module Mp_plain = struct
  type st = { data : int array; flag : int array; res : int array }

  let fresh () = { data = loc (); flag = loc (); res = Array.make 2 0 }

  let clear s =
    s.data.(slot) <- 0;
    s.flag.(slot) <- 0;
    s.res.(0) <- 0;
    s.res.(1) <- 0

  let test =
    Runner.Test
      {
        name = "MP publication (plain accesses)";
        doc = "same shape without atomics; the fence on plain stores should hold it";
        parties = 2;
        labels = [| "sawflag"; "data" |];
        radix = 2;
        init = fresh;
        reset = clear;
        control = false;
        thread =
          (fun s i ->
            if i = 0 then begin
              s.data.(slot) <- 1;
              s.flag.(slot) <- 1
            end
            else begin
              s.res.(0) <- s.flag.(slot);
              s.res.(1) <- s.data.(slot)
            end);
        observe = (fun s o -> o.(0) <- s.res.(0); o.(1) <- s.res.(1));
        forbidden = (fun o -> o.(0) = 1 && o.(1) = 0);
      }
end

module Sb = struct
  type st = { x : int Atomic.t; y : int Atomic.t; res : int array }

  let fresh () =
    {
      x = Atomic.make_contended 0;
      y = Atomic.make_contended 0;
      res = Array.make 2 0;
    }

  let clear s =
    Atomic.set s.x 0;
    Atomic.set s.y 0;
    s.res.(0) <- 0;
    s.res.(1) <- 0

  let test =
    Runner.Test
      {
        name = "SB (store buffering, atomic)";
        doc = "T0: x:=1; r0=y   T1: y:=1; r1=x   forbids r0=0,r1=0";
        parties = 2;
        labels = [| "r0"; "r1" |];
        radix = 2;
        init = fresh;
        reset = clear;
        control = false;
        thread =
          (fun s i ->
            if i = 0 then begin
              Atomic.set s.x 1;
              s.res.(0) <- Atomic.get s.y
            end
            else begin
              Atomic.set s.y 1;
              s.res.(1) <- Atomic.get s.x
            end);
        observe = (fun s o -> o.(0) <- s.res.(0); o.(1) <- s.res.(1));
        forbidden = (fun o -> o.(0) = 0 && o.(1) = 0);
      }
end

(* The only genuine control. Store-load is not ordered by the fence OCaml puts
   before plain stores, so nothing prevents this. *)
module Sb_plain = struct
  type st = { x : int array; y : int array; res : int array }

  let fresh () = { x = loc (); y = loc (); res = Array.make 2 0 }

  let clear s =
    s.x.(slot) <- 0;
    s.y.(slot) <- 0;
    s.res.(0) <- 0;
    s.res.(1) <- 0

  let test =
    Runner.Test
      {
        name = "SB control (plain accesses)";
        doc = "same without atomics; store-load is unordered by any fence here";
        parties = 2;
        labels = [| "r0"; "r1" |];
        radix = 2;
        init = fresh;
        reset = clear;
        control = true;
        thread =
          (fun s i ->
            if i = 0 then begin
              s.x.(slot) <- 1;
              s.res.(0) <- s.y.(slot)
            end
            else begin
              s.y.(slot) <- 1;
              s.res.(1) <- s.x.(slot)
            end);
        observe = (fun s o -> o.(0) <- s.res.(0); o.(1) <- s.res.(1));
        forbidden = (fun o -> o.(0) = 0 && o.(1) = 0);
      }
end

module Lb = struct
  type st = { x : int Atomic.t; y : int Atomic.t; res : int array }

  let fresh () =
    {
      x = Atomic.make_contended 0;
      y = Atomic.make_contended 0;
      res = Array.make 2 0;
    }

  let clear s =
    Atomic.set s.x 0;
    Atomic.set s.y 0;
    s.res.(0) <- 0;
    s.res.(1) <- 0

  let test =
    Runner.Test
      {
        name = "LB (load buffering, atomic)";
        doc = "T0: r0=x; y:=1   T1: r1=y; x:=1   forbids r0=1,r1=1";
        parties = 2;
        labels = [| "r0"; "r1" |];
        radix = 2;
        init = fresh;
        reset = clear;
        control = false;
        thread =
          (fun s i ->
            if i = 0 then begin
              s.res.(0) <- Atomic.get s.x;
              Atomic.set s.y 1
            end
            else begin
              s.res.(1) <- Atomic.get s.y;
              Atomic.set s.x 1
            end);
        observe = (fun s o -> o.(0) <- s.res.(0); o.(1) <- s.res.(1));
        forbidden = (fun o -> o.(0) = 1 && o.(1) = 1);
      }
end

(* Once a domain has seen the new value it must not see the old one again. *)
module Corr = struct
  type st = { x : int Atomic.t; res : int array }

  let fresh () = { x = Atomic.make_contended 0; res = Array.make 2 0 }

  let clear s =
    Atomic.set s.x 0;
    s.res.(0) <- 0;
    s.res.(1) <- 0

  let test =
    Runner.Test
      {
        name = "CoRR (read-read coherence)";
        doc = "T0: x:=1   T1: r0=x; r1=x   forbids r0=1,r1=0";
        parties = 2;
        labels = [| "r0"; "r1" |];
        radix = 2;
        init = fresh;
        reset = clear;
        control = false;
        thread =
          (fun s i ->
            if i = 0 then Atomic.set s.x 1
            else begin
              s.res.(0) <- Atomic.get s.x;
              s.res.(1) <- Atomic.get s.x
            end);
        observe = (fun s o -> o.(0) <- s.res.(0); o.(1) <- s.res.(1));
        forbidden = (fun o -> o.(0) = 1 && o.(1) = 0);
      }
end

(* If T1 saw T0's write and then published, T2 cannot see the publication
   without the original. *)
module Wrc = struct
  type st = { x : int Atomic.t; y : int Atomic.t; res : int array }

  let fresh () =
    {
      x = Atomic.make_contended 0;
      y = Atomic.make_contended 0;
      res = Array.make 3 0;
    }

  let clear s =
    Atomic.set s.x 0;
    Atomic.set s.y 0;
    for i = 0 to 2 do
      s.res.(i) <- 0
    done

  let test =
    Runner.Test
      {
        name = "WRC (write-to-read causality)";
        doc = "T0: x:=1  T1: r0=x; y:=1  T2: r1=y; r2=x   forbids r0=1,r1=1,r2=0";
        parties = 3;
        labels = [| "r0"; "r1"; "r2" |];
        radix = 2;
        init = fresh;
        reset = clear;
        control = false;
        thread =
          (fun s i ->
            match i with
            | 0 -> Atomic.set s.x 1
            | 1 ->
                s.res.(0) <- Atomic.get s.x;
                Atomic.set s.y 1
            | _ ->
                s.res.(1) <- Atomic.get s.y;
                s.res.(2) <- Atomic.get s.x);
        observe =
          (fun s o ->
            o.(0) <- s.res.(0);
            o.(1) <- s.res.(1);
            o.(2) <- s.res.(2));
        forbidden = (fun o -> o.(0) = 1 && o.(1) = 1 && o.(2) = 0);
      }
end

(* If the readers disagree about which write landed first, the writes were not
   seen in a single order by everyone. *)
module Iriw = struct
  type st = { x : int Atomic.t; y : int Atomic.t; res : int array }

  let fresh () =
    {
      x = Atomic.make_contended 0;
      y = Atomic.make_contended 0;
      res = Array.make 4 0;
    }

  let clear s =
    Atomic.set s.x 0;
    Atomic.set s.y 0;
    for i = 0 to 3 do
      s.res.(i) <- 0
    done

  let test =
    Runner.Test
      {
        name = "IRIW (independent reads, independent writes)";
        doc =
          "T0: x:=1  T1: y:=1  T2: r0=x,r1=y  T3: r2=y,r3=x   forbids the readers disagreeing";
        parties = 4;
        labels = [| "r0"; "r1"; "r2"; "r3" |];
        radix = 2;
        init = fresh;
        reset = clear;
        control = false;
        thread =
          (fun s i ->
            match i with
            | 0 -> Atomic.set s.x 1
            | 1 -> Atomic.set s.y 1
            | 2 ->
                s.res.(0) <- Atomic.get s.x;
                s.res.(1) <- Atomic.get s.y
            | _ ->
                s.res.(2) <- Atomic.get s.y;
                s.res.(3) <- Atomic.get s.x);
        observe =
          (fun s o ->
            o.(0) <- s.res.(0);
            o.(1) <- s.res.(1);
            o.(2) <- s.res.(2);
            o.(3) <- s.res.(3));
        forbidden = (fun o -> o.(0) = 1 && o.(1) = 0 && o.(2) = 1 && o.(3) = 0);
      }
end

module Cas = struct
  type st = { x : int Atomic.t; res : int array }

  let fresh () = { x = Atomic.make_contended 0; res = Array.make 2 0 }

  let clear s =
    Atomic.set s.x 0;
    s.res.(0) <- 0;
    s.res.(1) <- 0

  let test =
    Runner.Test
      {
        name = "CAS (mutual exclusion)";
        doc = "both cas 0 -> own tag; exactly one wins and the final value is its tag";
        parties = 2;
        labels = [| "won0"; "won1"; "final" |];
        radix = 3;
        init = fresh;
        reset = clear;
        control = false;
        thread =
          (fun s i ->
            let won = Atomic.compare_and_set s.x 0 (i + 1) in
            s.res.(i) <- (if won then 1 else 0));
        observe =
          (fun s o ->
            o.(0) <- s.res.(0);
            o.(1) <- s.res.(1);
            o.(2) <- Atomic.get s.x);
        forbidden =
          (fun o ->
            (o.(0) = 1 && o.(1) = 1)
            || (o.(0) = 0 && o.(1) = 0)
            || (o.(0) = 1 && o.(2) <> 1)
            || (o.(1) = 1 && o.(2) <> 2));
      }
end

(* The two returned values must differ, and the total must account for both. *)
module Faa = struct
  type st = { x : int Atomic.t; res : int array }

  let fresh () = { x = Atomic.make_contended 0; res = Array.make 2 0 }

  let clear s =
    Atomic.set s.x 0;
    s.res.(0) <- 0;
    s.res.(1) <- 0

  let test =
    Runner.Test
      {
        name = "FAA (fetch and add)";
        doc = "both fetch_and_add 1; the old values must differ and the final must be 2";
        parties = 2;
        labels = [| "got0"; "got1"; "final" |];
        radix = 3;
        init = fresh;
        reset = clear;
        control = false;
        thread = (fun s i -> s.res.(i) <- Atomic.fetch_and_add s.x 1);
        observe =
          (fun s o ->
            o.(0) <- s.res.(0);
            o.(1) <- s.res.(1);
            o.(2) <- Atomic.get s.x);
        forbidden = (fun o -> o.(0) = o.(1) || o.(2) <> 2);
      }
end

(* Exactly one domain must see the initial value, and the survivor's tag must
   be what is left behind. *)
module Exchange = struct
  type st = { x : int Atomic.t; res : int array }

  let fresh () = { x = Atomic.make_contended 0; res = Array.make 2 0 }

  let clear s =
    Atomic.set s.x 0;
    s.res.(0) <- 0;
    s.res.(1) <- 0

  let test =
    Runner.Test
      {
        name = "XCHG (exchange)";
        doc = "T0 swaps in 1, T1 swaps in 2; exactly one sees 0";
        parties = 2;
        labels = [| "got0"; "got1"; "final" |];
        radix = 3;
        init = fresh;
        reset = clear;
        control = false;
        thread = (fun s i -> s.res.(i) <- Atomic.exchange s.x (i + 1));
        observe =
          (fun s o ->
            o.(0) <- s.res.(0);
            o.(1) <- s.res.(1);
            o.(2) <- Atomic.get s.x);
        forbidden =
          (fun o ->
            let saw_initial =
              (if o.(0) = 0 then 1 else 0) + if o.(1) = 0 then 1 else 0
            in
            saw_initial <> 1
            || (o.(0) = 0 && not (o.(1) = 1 && o.(2) = 2))
            || (o.(1) = 0 && not (o.(0) = 2 && o.(2) = 1)));
      }
end

let all =
  [
    Mp.test;
    Mp_plain.test;
    Sb.test;
    Sb_plain.test;
    Lb.test;
    Corr.test;
    Wrc.test;
    Iriw.test;
    Cas.test;
    Faa.test;
    Exchange.test;
  ]
