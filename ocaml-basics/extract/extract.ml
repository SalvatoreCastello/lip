(* Estrattore degli elementi, itera riducendo il counter finchè non è a 0, come
   arriva a 0, restituisce l'elemento (emula un ciclo for) *)
let rec extractor i = function
    [] -> failwith "Index out of bounds."
  | h::l -> if i=0 then h else extractor (i-1) l;;

(* Eliminatore degli elementi, itera riducendo il counter finchè non è a 0, come
   arriva a 0, restituisce la lista originale a meno dell'elemento in testa al
   momento del counter=0 *)
let rec pop i = function
    [] -> []
  | h::l -> if i=0 then l else h::pop (i-1) l;;

(* Funzione richiesta, restituisce la coppia di elemento estratto e lista *)
let rec extract i l = (extractor i l,pop i l);;