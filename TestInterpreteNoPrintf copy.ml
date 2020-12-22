(*Leonardo Scoppitto, MATRICOLA: 545614, CORSO:B*)
type ide = string;;

(*ambiente polimorfo*)
type 't env = ide -> 't;;
let emptyenv (v : 't) = function x -> v;;
let applyenv (r : 't env) (i : ide) = r i;;
let bind (r : 't env) (i : ide) (v : 't) = function x -> if x = i then v else applyenv r x;;

type exp = 
		(*Valori primitivi*)
		  Eint of int 
		| Ebool of bool 
		| Estring of string
		(*Operatori già inclusi nel linguaggio*)
		| Den of ide 
		| Prod of exp * exp 
		| Sum of exp * exp 
		| Diff of exp * exp 
		| Eq of exp * exp 
		| Minus of exp 
		| IsZero of exp
		| IsPositive of exp (*Operazione aggiuntiva per eseguire il test*)
		| Or of exp * exp 
		| And of exp * exp 
		| Not of exp 
		| Ifthenelse of exp * exp * exp 
		| Let of ide * exp * exp 
		| Fun of ide * exp 
		| FunCall of exp * exp 
		| Letrec of ide * exp * exp

		(*Operazioni di carattere funzionale*)
		| For_all of exp * exp
		| Exists of exp * exp
		| Filter of exp * exp
		| Map of exp * exp
           
		(*Operatori di base sugli insiemi*)
		| Empty of ide
		| Singleton of exp * ide
		| Union of exp * exp
		| Intersection of exp * exp
		| Difference of exp * exp
		| Insert of exp * exp
		| Rm of exp * exp
		| IsEmpty of exp
		| IsIn of exp * exp
		| IsSubset of exp * exp
		| Getmin of exp 
		| Getmax of exp 
  
    (*tipi esprimibili*)
type evT = 
    Int of int 
  | Bool of bool 
  | String of string
  | Unbound 
  | FunVal of evFun 
  | RecFunVal of ide * evFun

    (*Insieme come tipo esprimibile*)
  | SetVal of (evT list) * ide (*Un set è una coppia formata da una lista e un elemento di tipo ide*)
and evFun = ide * exp * evT env;;





(*rts*)
(*type checking*)
let typecheck (s : string) (v : evT) : bool = match s with
    "int" -> (match v with
        Int(_) -> true 
      | _ -> false) 
  | "bool" -> (match v with
        Bool(_) -> true 
      | _ -> false) 
  | "string" -> (match v with
        String(_) -> true
      | _ ->false)
  | _ -> failwith("not a valid type");;

(*funzioni primitive*)
let prod x y = if (typecheck "int" x) && (typecheck "int" y)
  then (match (x,y) with (Int(n),Int(u)) -> Int(n*u))
  else failwith("Type error");;

let sum x y = if (typecheck "int" x) && (typecheck "int" y)
  then (match (x,y) with
        (Int(n),Int(u)) -> Int(n+u))
  else failwith("Type error");;

let diff x y = if (typecheck "int" x) && (typecheck "int" y)
  then (match (x,y) with (Int(n),Int(u)) -> Int(n-u))
  else failwith("Type error");;

let eq x y = if (typecheck "int" x) && (typecheck "int" y)
  then (match (x,y) with (Int(n),Int(u)) -> Bool(n=u))
  else failwith("Type error");;

let minus x = if (typecheck "int" x) 
  then (match x with Int(n) -> Int(-n))
  else failwith("Type error");;

let iszero x = if (typecheck "int" x)
  then (match x with Int(n) -> Bool(n=0))
  else failwith("Type error");;

let vel x y = if (typecheck "bool" x) && (typecheck "bool" y)
  then (match (x,y) with (Bool(b),Bool(e)) -> (Bool(b||e)))
  else failwith("Type error");;

let et x y = if (typecheck "bool" x) && (typecheck "bool" y)
  then (match (x,y) with (Bool(b),Bool(e)) -> Bool(b&&e))
  else failwith("Type error");;

let non x = if (typecheck "bool" x)
  then (match x with
        Bool(true) -> Bool(false) 
      | Bool(false) -> Bool(true))
  else failwith("Type error");;
  
let ispositive x = if (typecheck "int" x) (*Implementazione di IsPositive *)
  then (match x with Int(n) -> Bool(n=0 || n > 0))
  else failwith("Type error");;

let toexp (a : evT) = match a with		(*Funzione di supporto per la conversione di tipo*)
  | Int(q) -> Eint(q)
  | Bool(q) -> Ebool(q)
  | String(q) -> Estring(q)
  | _-> failwith("Convert error");;		


(*Alcune funzioni base *)

  (*Funzione di supporto per sublist*)
let rec subaux (l : 'a list)(m : 'a list) : bool = match l, m with 
    [], _ -> true
  | _, [] -> false
  | hdl::tll, hdm::tlm -> hdl = hdm && subaux tll tlm;; 

  (*Funzione per verificare se una lista è contenuta in un'altra*)
let rec sublist (l : 'a list)(m : 'a list) : bool = match l, m with 
    [], _ -> true
  | _, [] -> false
  | hdl::_, hdm::tlm -> hdl = hdm && subaux l m || sublist l tlm;;											

  (*Funzione per controllare se un elemento appartiene a una lista*)
let rec contains (toSearch : 'a)(l : 'a list) : bool = match l with 
    [] -> false
  | element::tail -> element = toSearch || contains toSearch tail;;

  (*Funzione di supporto per list_as_set che rimuove un elemento da una lista*)
let rec remove_from_list (element : 'a)(l : 'a list) : 'a list = match l with
    [] -> []
  | head::tail -> if head = element then remove_from_list element tail 
					 else head::(remove_from_list element tail);;

  (*Funzione che rimuove i duplicati da una lista*)
let rec list_as_set (l : 'a list) : 'a list = match l with 
    [] -> []
  | head::tail -> if (contains head tail) then head::list_as_set((remove_from_list head tail)) 
					 else head::(list_as_set tail);;

(*interprete*)
let rec eval (e : exp) (r : evT env) : evT = match e with
(*Operazioni incluse nell'interprete*)
		  Eint n -> Int n 
  		| Ebool b -> Bool b 
  		| IsPositive a -> ispositive (eval a r) (*Funzione aggiuntiva che restituisce Bool(true) se a >= 0*)
		| Estring a -> String a
		| IsZero a -> iszero (eval a r) 
		| Den i -> applyenv r i 
		| Eq(a, b) -> eq (eval a r) (eval b r) 
		| Prod(a, b) -> prod (eval a r) (eval b r) 
		| Sum(a, b) -> sum (eval a r) (eval b r) 
		| Diff(a, b) -> diff (eval a r) (eval b r) 
		| Minus a -> minus (eval a r) 
		| And(a, b) -> et (eval a r) (eval b r) 
		| Or(a, b) -> vel (eval a r) (eval b r) 
		| Not a -> non (eval a r) 
		| Ifthenelse(a, b, c) -> let g = (eval a r) in
					if (typecheck "bool" g) 
					then (if g = Bool(true) then (eval b r) else (eval c r))
					else failwith ("nonboolean guard") 
						| Let(i, e1, e2) -> eval e2 (bind r i (eval e1 r)) 
						| Fun(i, a) -> FunVal(i, a, r) 
						| FunCall(f, eArg) -> let fClosure = (eval f r) in
					(match fClosure with
						FunVal(arg, fBody, fDecEnv) -> eval fBody (bind fDecEnv arg (eval eArg r)) 
					| RecFunVal(g, (arg, fBody, fDecEnv)) -> let aVal = (eval eArg r) in
						let rEnv = (bind fDecEnv g fClosure) in
						let aEnv = (bind rEnv arg aVal) in
															eval fBody aEnv 
					|	_ -> failwith("non functional value")) 
						
		| Letrec(f, funDef, letBody) ->(match funDef with
						  Fun(i, fBody) -> let r1 = (bind r f (RecFunVal(f, (i, fBody, r)))) in eval letBody r1 
          
						| _ -> failwith("non functional def"))
    
		(*=================== operazioni di base ====================*)
          (*Creazione insiemi*)
        (*Inizializza l'insieme vuoto di tipo "type_"*)
		| Empty(type_) ->( match type_ with 
				"int" -> SetVal([], type_)
			| "bool" -> SetVal([], type_)
			| "string" -> SetVal([], type_)
			| _ -> failwith("Not a valid type"))

		(*Inizializza un insieme di un elemento di tipo "type_"*)
		| Singleton(a, type_) -> if typecheck type_ (eval a r) then SetVal((eval a r)::[], type_)
      else failwith("Wrong type") 
          
        (*Operazioni elementari*)
		| Union(s1, s2) -> (match (eval s1 r), (eval s2 r) with
	  							(*Unisco le liste e dopodiché restituisco un set contente la lista finale senza duplicati*)
								SetVal(l1, t1), SetVal(l2, t2) -> if t1 <> t2 then failwith("Sets are not the same type") 
												else let final_list = l1@l2 in SetVal(list_as_set final_list, t1)
							| _,SetVal(_,_) -> failwith("s1 is not a Set")
							| SetVal(_,_),_ -> failwith("s2 is not a Set")
							| _,_ -> failwith("Not a valid pair of Sets"))
          
		| Intersection(s1, s2) -> (match (eval s1 r), (eval s2 r) with
		SetVal(l1, t1), SetVal(l2, t2) ->( if t1 <> t2 then failwith("Sets are not the same type") 
									(*Cerco ogni elemento di l1 in l2 e se lo trovo, lo aggiungo al set da restituire*)
                                           else let rec intersect l1 l2 = match l1, l2 with
                                                 head::tail, lista -> if (contains head lista) then head::(intersect tail lista) 
                                                   else (intersect tail lista)
                                               | [], lista -> []
                                               | lista, [] -> []	
                                             in SetVal(intersect l1 l2, t1))
      | _,SetVal(_,_) -> failwith("s1 is not a Set")
      | SetVal(_,_),_ -> failwith("s2 is not a Set")
      | _,_ -> failwith("Not a valid pair of Sets"))

		| Difference(s1, s2) ->( match (eval s1 r), (eval s2 r) with
		SetVal(l1, t1), SetVal(l2, t2) ->( if t1 <> t2 then failwith("Sets are not the same type") 
									(*Rimuova l1, se presenti, gli elementi di l2*)
                                           else let rec difference l1 l2 = match l1, l2 with
                                                 lista, head::tail -> difference (remove_from_list head lista) tail
                                               | [], lista -> []
                                               | lista, [] -> lista
                                             in SetVal(difference l1 l2, t1))

      | _,SetVal(_,_) -> failwith("s1 is not a Set")
      | SetVal(_,_),_ -> failwith("s2 is not a Set")
      | _,_ -> failwith("Not a valid pair of Sets"))

  				(*Operazioni base di cui è richiesta l'implementazione*)
		| Insert (s, toAdd) -> (
      match eval s r with
        SetVal(l, t) -> if (typecheck t (eval toAdd r)) then (if not(contains (eval toAdd r) l) then SetVal((eval toAdd r)::l, t)
                                                              else failwith("Already Existing")) else failwith ("Wrong type")
      | _-> failwith("Not a Set"))

  | IsIn (s, query) -> (
      match eval s r with
        SetVal(l, t) -> if (typecheck t (eval query r)) then Bool(contains (eval query r) l) else failwith ("Wrong type")
      | _-> failwith("Not a Set"))
    
  | Rm (s, toDel) -> (
      match eval s r with
        SetVal(l, t) -> if (typecheck t (eval toDel r)) then SetVal(delete toDel l, t) else failwith ("Wrong type")
      | _-> failwith("Not a Set"))

  | Getmin (s) -> (match eval s r with
        SetVal(l, t) -> findmin l
      | _-> failwith("Not a Set"))
    
		| Getmax (s) ->( match eval s r with
        SetVal(l, t) -> findmax l
      | _-> failwith("Not a Set"))

		| IsEmpty (s) -> (match eval s r with
        SetVal(l, t) -> (match l with
            [] -> Bool(true)
          | h::t -> Bool(false)
          |_ -> failwith("Not a valid Set"))
      | _ -> failwith("Not a Set"))

		| IsSubset (s1, s2) -> (match (eval s1 r, eval s2 r) with
        (SetVal(l, t1), SetVal(m, t2)) -> if (t1 = t2) then Bool(sublist l m) 
										else failwith("Sets are not the same type")
      | _ -> failwith("Not a valid enty"))

  				(*Operazioni di carattere funzionale di cui è richiesta l'implementazione*)
		| For_all(f, s) -> ( match f, eval s r with
        f, SetVal([], t) -> failwith("Set is Empty") (*Se il set è vuoto, non ho elementi su cui valutare il predicato*)
      | f, SetVal(l, t) ->  let fClosure = (eval f r) 
          in let apply (f : evT) (v : evT) : evT = (match f with
                FunVal(arg, fBody, fDecEnv) -> eval fBody (bind fDecEnv arg v) 
              | RecFunVal(g, (arg, fBody, fDecEnv)) -> let rEnv = (bind fDecEnv g f) in
                  let aEnv = (bind rEnv arg v) in
                  eval fBody aEnv 
              | _ -> failwith("non functional value"))
          in let rec forall  (f : evT) (lista : evT list) : evT = match lista with
                [] -> Bool(true) (*Se arrivo in fondo alla lista restituisco true*)
              | hd::tl -> if ((apply f hd) = Bool(true)) then forall f tl 
				  else Bool(false) (*Nel caso trovassi un elemento che non rispetta la proprietà, mi fermo*)
										in forall fClosure l
      | f, _ -> failwith("Not a valid set"))
		| Exists(f, s) -> ( match f, eval s r with
        f, SetVal([], t) -> failwith("Set is Empty")
      | f, SetVal(l, t) ->  let fClosure = (eval f r) 
          in let apply (f : evT) (v : evT) : evT = (match f with
                FunVal(arg, fBody, fDecEnv) -> eval fBody (bind fDecEnv arg v) 
              | RecFunVal(g, (arg, fBody, fDecEnv)) -> let rEnv = (bind fDecEnv g f) in
                  let aEnv = (bind rEnv arg v) in
                  eval fBody aEnv 
              |	_ -> failwith("non functional value"))
          in let rec exists  (f : evT) (lista : evT list) : evT = match lista with		
                [] -> Bool(false) (*Se arrivo in fondo alla lista vuol dire che non ho elementi che rispettano il predicato*)
              | hd::tl -> if ((apply f hd) = Bool(true)) then Bool(true) 
                  else exists f tl (*Se trovo un elemento che soddisfa il predicato, restituisco true e mi fermo*)
										in exists fClosure l
      | f, _ -> failwith("Not a valid set"))

		| Filter(f, s) -> ( match f, eval s r with
        f, SetVal([], t) -> failwith("Set is Empty")
      | f, SetVal(l, t) ->  let fClosure = (eval f r) 
          in let apply (f : evT) (v : evT) : evT = (match f with
                FunVal(arg, fBody, fDecEnv) -> eval fBody (bind fDecEnv arg v) 
              | RecFunVal(g, (arg, fBody, fDecEnv)) -> let rEnv = (bind fDecEnv g f) in
                  let aEnv = (bind rEnv arg v) in
                  eval fBody aEnv 
              |	_ -> failwith("non functional value"))
          in let rec filter  (f : evT) (lista : evT list) : (evT list) = match lista with
                [] -> []
              | hd::tl -> if ((apply f hd) = Bool(true)) then hd::(filter f tl) 
                  else filter f tl (*Popolo una lista di elementi che rispettano il predicato e li inserisco in un nuovo Set*)
										in SetVal(filter fClosure l, t)
      | f, _ -> failwith("Not a valid set"))

		| Map(f, s) -> ( match f, eval s r with
        f, SetVal([], t) -> failwith("Set is Empty")
      | f, SetVal(l, t) ->  let fClosure = (eval f r) 
          in let apply (f : evT) (v : evT) : evT = (match f with
                FunVal(arg, fBody, fDecEnv) -> eval fBody (bind fDecEnv arg v) 
              | RecFunVal(g, (arg, fBody, fDecEnv)) -> let rEnv = (bind fDecEnv g f) in
                  let aEnv = (bind rEnv arg v) in
                  eval fBody aEnv 
              |	_ -> failwith("non functional value"))
          in let rec map (f : evT) (lista : evT list) : (evT list) = match lista with
                [] -> []
              | hd::tl -> (apply f hd)::(map f tl) (*Popolo una lista di elementi e = f(hd) e la inserisco in una nuovo set*)
										in SetVal(list_as_set(map fClosure l), t)
      | f, _ -> failwith("Not a valid set"))
    
		(*Funzioni di appoggio per le operazioni base*)
and findmin (l : evT list) : evT = match l with
		  [] -> Int(0)
		| [element] -> element
		| element::tail -> let smallest = findmin tail in if element < smallest then element else smallest

and findmax (l : evT list) : evT = match l with
		  [] -> Int(0)
		| [element] -> element
		| element::tail -> let biggest = findmax tail in if element > biggest then element else biggest

and delete (toDelete : exp)(l : evT list) : (evT list) = match l with 
		  [] -> []
		| element::tail -> if toDelete =  (toexp element) then (delete toDelete tail) 
      else element::(delete toDelete tail)													
;; 
	(*===================== Fine Interprete =====================*)

(* =====================  TESTS  ===================== *)

let env0 = emptyenv Unbound;;

let intset = Singleton(Eint(75), "int");;
let intset = Insert(intset, Eint(3));;
let intset = Insert(intset, Eint(20));;
eval intset env0;;

let intset1 = Singleton(Eint(82), "int");;
let intset1 = Insert(intset1, Eint(34));;
let intset1 = Insert(intset1, Eint(20));;
eval intset1 env0;;

let unione = Union(intset, intset1);;
eval unione env0;;

let intersezione = Intersection(intset, intset1);;
eval intersezione env0;;

let differenza = Difference(intset, intset1);;
eval differenza env0;;

let stringset = Empty("string");;
let isemp = IsEmpty(stringset);;
eval isemp env0;;

let stringset = Insert(stringset, Estring("OCaml"));;
let stringset = Insert(stringset, Estring("Interprete"));;
let stringset = Insert(stringset, Estring("Delfino"));;
eval stringset env0;;

let min = Getmin(stringset);;
eval min env0;;

let max = Getmax(stringset);;
eval max env0;;

let stringset = Rm(stringset, Estring("Delfino"));;
eval stringset env0;;

let istrue = let stringa = Estring("OCaml") in IsIn(stringset, stringa);;
eval istrue env0;;

let istrue = let stringa = Estring("Delfino") in IsIn(stringset, stringa);;
eval istrue env0;;

let stringset1 = Singleton(Estring("OCaml"), "string");;
eval stringset1 env0;;

let isSub = IsSubset(stringset1, stringset);;
eval isSub env0;;

let isSub = IsSubset(stringset, stringset1);;
eval isSub env0;;

let minmax = Singleton(Eint(1), "int");;
let minmax = Insert(minmax, Eint(10));;
let minmax = Insert(minmax, Eint(1123));;
let minmax = Insert(minmax, Eint(25));;
let minmax = Insert(minmax, Eint(324));;
eval minmax env0;;

let min = Getmin(minmax);;
eval min env0;;

let max = Getmax(minmax);;
eval max env0;;

let set0 = Singleton(Eint(1), "int");;
let set0 = Insert(set0, Eint(3));;
let set0 = Insert(set0, Eint(5));;
eval set0 env0;;

let positive = Fun("element", IsPositive(Den "element"));;
eval positive env0;;

let all = For_all(positive, set0);;
eval all env0;;

let set0 = Insert(set0, Eint(-7));;
eval set0 env0;;

let all = For_all(positive, set0);;
eval all env0;;

let is3 = Fun("element", Eq(Den ("element"), Eint(3)));;
let is10 = Fun("element", Eq(Den ("element"), Eint(10)));;
let exists3 = Exists(is3, set0);;
eval exists3 env0;;

let exists10 = Exists(is10, set0);;
eval exists10 env0;;

let isnegative = Fun("element",Not(IsPositive(Den "element")));;
let neg = Filter(isnegative, set0);;
eval neg env0;;

let double = Fun("element", Prod(Den "element", Eint(2)));;
let twice = Map(double, set0);;
eval twice env0;;