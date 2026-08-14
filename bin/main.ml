let contains needle hay =
  let n = String.length needle and h = String.length hay in
  n = 0 || (let rec go i = i + n <= h && (String.sub hay i n = needle || go (i + 1)) in go 0)

let () =
  let iterations = ref 200_000 in
  let only = ref "" in
  Arg.parse
    [ "-n", Arg.Set_int iterations, "N iterations per test (default 200000)";
      "-only", Arg.Set_string only, "NAME run only tests whose name contains NAME" ]
    (fun _ -> ())
    "litmus [-n ITERATIONS] [-only NAME]";
  Printf.printf "OCaml memory model litmus tests\n";
  Printf.printf "%d domains available\n\n" (Domain.recommended_domain_count ());
  (* A control firing is the good outcome, so it is not a failure. Only a real
     test producing a forbidden outcome means the model was broken. *)
  let cores = Domain.recommended_domain_count () in
  let failures = ref 0 and controls_fired = ref 0 and controls = ref 0 in
  let skipped = ref 0 in
  List.iter
    (fun (Temoin.Test spec as t) ->
       if contains !only spec.Temoin.name then
       if spec.Temoin.parties > cores then begin
         (* A spin barrier with more parties than cores crawls, since a
            spinning domain holds a core the missing party needs. *)
         incr skipped;
         Printf.printf "%s
  skipped, needs %d domains and this machine has %d

"
           spec.Temoin.name spec.Temoin.parties cores
       end else begin
         let r = Temoin.run ~iterations:!iterations t in
         Temoin.report t r;
         if spec.Temoin.control then begin
           incr controls;
           if r.Temoin.violations > 0 then incr controls_fired
         end else if r.Temoin.violations > 0 then incr failures
       end)
    Tests.all;
  if !skipped > 0 then
    Printf.printf "%d test(s) skipped for want of cores
" !skipped;
  if !controls > 0 && !controls_fired = 0 then
    print_endline
      "warning: no control fired, so this run cannot show a violation even if one existed";
  if !failures = 0 then print_endline "all tests ok"
  else (Printf.printf "%d test(s) violated the model\n" !failures; exit 1)
