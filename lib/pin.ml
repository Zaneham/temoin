(* Where the domains run. Left to the scheduler, results drift between runs on
   the same machine, and a relaxation that only shows up across cores looks
   like flakiness rather than a finding. *)

external pin_self : int -> int = "temoin_pin_self" [@@noalloc]
external pinning_supported : unit -> int = "temoin_pinning_supported"
  [@@noalloc]

let supported () = pinning_supported () = 1

(* sysfs disagrees with itself on real hardware. One POWER box reports a
   package id of -1 for all 192 cpus, another claims 64 packages for 64 cpus,
   and a riscv board repeats core ids while denying those cpus are siblings.
   thread_siblings_list was the only field that matched reality on all three,
   so it is the only one trusted here. *)
let siblings_path i =
  Printf.sprintf "/sys/devices/system/cpu/cpu%d/topology/thread_siblings_list" i

let present_path = "/sys/devices/system/cpu/present"

(* One line, and never by length. A sysfs file reports a size of 4096 whatever
   it actually holds, so asking for that many bytes just hits end of file. *)
let read_line path =
  match open_in path with
  | exception Sys_error _ -> None
  | ic ->
      let r =
        match input_line ic with
        | s -> Some (String.trim s)
        | exception End_of_file -> None
      in
      close_in_noerr ic;
      r

(* The sysfs cpulist form, so 0-7 or 0,4 or 0-3,8-11. Ranges are bounded so a
   garbled file cannot make us loop forever. *)
let max_range = 4096

let parse_cpulist s =
  let out = ref [] in
  List.iter
    (fun part ->
      match String.index_opt part '-' with
      | None -> (
          match int_of_string_opt part with
          | Some v when v >= 0 -> out := v :: !out
          | _ -> ())
      | Some i ->
          let a = int_of_string_opt (String.sub part 0 i) in
          let b =
            int_of_string_opt (String.sub part (i + 1) (String.length part - i - 1))
          in
          (match (a, b) with
          | Some a, Some b when a >= 0 && b >= a && b - a < max_range ->
              for v = b downto a do
                out := v :: !out
              done
          | _ -> ()))
    (String.split_on_char ',' s);
  List.sort_uniq compare !out

type t = {
  ncpus : int;
  (* per cpu, the cpus sharing its physical core, itself included *)
  siblings : int array array;
  usable : bool;
}

let unknown = { ncpus = 0; siblings = [||]; usable = false }

let read () =
  if not (supported ()) then unknown
  else
    match read_line present_path with
    | None -> unknown
    | Some s -> (
        match List.rev (parse_cpulist s) with
        | [] -> unknown
        | top :: _ ->
            let n = top + 1 in
            if n <= 0 || n > max_range then unknown
            else begin
              let sib = Array.make n [||] in
              let ok = ref true in
              for i = 0 to n - 1 do
                match read_line (siblings_path i) with
                | Some s -> sib.(i) <- Array.of_list (parse_cpulist s)
                | None -> ok := false
              done;
              { ncpus = n; siblings = sib; usable = !ok }
            end)

type placement =
  | Anywhere  (* let the scheduler decide, which is the old behaviour *)
  | Spread  (* one party per physical core *)
  | Same_core  (* parties packed onto the SMT threads of one core *)

let describe = function
  | Anywhere -> "unpinned"
  | Spread -> "one party per physical core"
  | Same_core -> "all parties on one physical core"

(* A cpu per party, or None when the machine cannot express the placement. It
   is better to say so than to resolve to something that only looks right. *)
let resolve t placement parties =
  if parties <= 0 then None
  else
    match placement with
    | Anywhere -> None
    | _ when not t.usable -> None
    | Spread ->
        let seen = Hashtbl.create 64 in
        let picked = ref [] and count = ref 0 in
        for i = 0 to t.ncpus - 1 do
          if !count < parties then begin
            let key =
              if Array.length t.siblings.(i) > 0 then t.siblings.(i).(0) else i
            in
            if not (Hashtbl.mem seen key) then begin
              Hashtbl.add seen key ();
              picked := i :: !picked;
              incr count
            end
          end
        done;
        if !count = parties then Some (Array.of_list (List.rev !picked)) else None
    | Same_core ->
        let found = ref None in
        for i = 0 to t.ncpus - 1 do
          if !found = None && Array.length t.siblings.(i) >= parties then
            found := Some (Array.sub t.siblings.(i) 0 parties)
        done;
        !found
