(* Se il numero è 0 restituisce true, altrimenti verifica:
   1) Nel caso sia dispari controlla che il counter sia dispari e il numero
     sia dispari, se lo sono effettua la ricorsione sul counter incrementato
     e il numero/10
   2) Nel caso sia pari controlla che il counter sia pari e il numero
     sia pari, se lo sono effettua la ricorsione sul counter incrementato
     e il numero/10 *)
let rec alt_evenrec count n = 
  if n=0 then true else if
    count mod 2 = 1 && n mod 2 = 1 then alt_evenrec (count+1) (n/10) else if
    count mod 2 = 0 && n mod 2 = 0 then alt_evenrec (count+1) (n/10) else false;;

(* Verifica che il numero iniziale sia 0 o un numero pari, se lo è allora
   chiama la funzione ricorsiva passandogli il numero/10 con un counter
   incrementato, altrimenti restituisce false. *)
let rec alt_even n = 
  if n=0 || n mod 2 = 0 then alt_evenrec 1 (n/10) else false;;