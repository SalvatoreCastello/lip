open ArithexprLib.Main
 
let tests = [
  ("false",Bool false);
  ("if true then false else true",Bool false);
  ("if true then (if true then false else true) else (if true then true else false)",Bool false);
  ("if (if false then false else false) then (if false then true else false) else (if true then false else true)",Bool false);
  ("if (if (if false then false else false) then (if false then true else false) else (if true then false else true)) then (if false then true else false) else (if true then false else true)",Bool false);
  ("not true or true",Bool true);
  ("not true and false",Bool false);
  ("false and false or true",Bool true);
  ("true or false and false",Bool true);
  ("if true then true else false and false",Bool true);
  ("if true then false else false or true",Bool false);
  ("succ 0",Nat 1);
  ("succ succ succ pred pred succ succ pred succ pred succ 0", Nat 3);
  ("iszero pred succ 0", Bool true);
  ("iszero pred succ 0 and not iszero succ pred succ 0", Bool true);
]

let%test _ = List.fold_left
    (fun b (s,v) ->
       print_string (s ^ " => ");
       let b' = ((s |> parse |> eval) = v) in
       print_string (string_of_val v);
       print_string (" " ^ (if b' then "[OK]" else "[NO]"));
       print_newline();
       b && b')
    true
    tests

