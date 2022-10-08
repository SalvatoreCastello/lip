(* Verifica che all'interno della lista ci siano elementi potenzialmente crushabili *)
let rec iscrushable = function
  | [] -> false
  | h::[] -> false
  | h1::h2::l -> if h1=h2 then true else iscrushable (h2::l);;

(* Verifica che l'elemento successivo alla testa sia uguale alla testa stessa *)
let rec isthereanotherone = function
  | [] -> false
  | h1::[] -> false
  | h1::h2::l -> h1=h2 ;;

(* Verifica le occorrenze dell'elemento in testa con i successivi, termina con il primo elemento diverso dalla testa *)
let rec count y = function
  | [] -> 0
  | h::[] -> 0
  | h1::h2::[] -> if h1=h2 then y+2 else y
  | h1::h2::l -> if h1=h2 then count (y+1) (h1::l) else y ;;

(* Rimuove gli 'i'-esimi elementi da una lista *)
let rec remove i = function
  | [] -> if i=0 then [] else failwith "Errore."
  | h::l -> if i=0 then h::l else remove (i-1) l;; 

(* Restituisce una lista crushata, nel caso di più di 2 elementi nella lista, 
   Verifica che ce ne siano altri uguali alla testa e successivamente li rimuove
   contando le occorrenze uguali alla testa fermandosi al primo elemento diverso,
   altrimenti se non ci sono altri elementi crushabili oltre quei 2, itera 
   controllando il resto della lista.
   Nel caso non ci siano elementi crushabili in testa, appende l'elemento in testa
   all'inizio della lista e continua a iterare l'algoritmo sul resto della lista.*)
let rec crushrec = function
  | [] -> []
  | h1::[] -> h1::[]
  | h1::h2::[] -> if h1=h2 then [] else (h1::h2::[])
  | h1::h2::l when h1=h2 -> if isthereanotherone (h2::l) then remove (count (1) (h1::h2::l)) (h1::h2::l) else crushrec l
  | h1::h2::l -> h1::crushrec (h2::l) ;; 

(* Verifica che una lista sia crushabile, se lo è effettua il crush e riverifica
   che sia crushabile finchè non sarà totalmente crushata. *)
let rec crush l = if iscrushable l then crush(crushrec l) else l;;