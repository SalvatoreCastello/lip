let rec alt = function 
  | [] -> true
  | h1::[] -> true
  | h1::h2::[] -> true
  | h1::h2::h3::[] -> (h1>h2 && h2<h3) || (h1<h2 && h2>h3)
  | h1::h2::h3::l -> if (h1>h2 && h2<h3) || (h1<h2 && h2>h3) then alt (h2::h3::l) else false;; 

let rec intervallo x y = if not((x+1)=y) then (x+1)::[] @ intervallo (x+1) y else [];; 

let rec has x = function
  | [] -> false
  | h::l -> if h=x then true else has x l;;

(* Al primo elemento dell'intervallo non contenuto nella lista dopo la coppia, rimanda subito false *)
let rec isallonanotherlist lista1 lista2 = match lista2 with
  | [] -> true
  | h::lista2 -> if has h lista1 then isallonanotherlist lista1 lista2 else false ;;

let rec isnettable = function
  | [] -> true
  | h1::[] -> true
  | h1::h2::[] -> true
  | h1::h2::h3::l when h2>h1 && h2>h3 -> if isallonanotherlist (h3::l) (intervallo h1 h2) then false else isnettable (h3::l)
  | h1::h2::h3::l when h2<h1 && h2<h3 -> if isallonanotherlist (h3::l) (intervallo h2 h1) then false else isnettable (h3::l)
  | h1::h2::h3::l -> false;;

let pingpong l = if alt l then (isnettable l) else false;; 

let l0 = [1;5;2;5;1;6];;
let l1 = [1;5;2;5;4;3];;
let l2 = [1;5;2;3;2;4];;
let l3 = [3;1;4;2;5;3];;

pingpong l0;;
pingpong l1;;
pingpong l2;;
pingpong l3;;