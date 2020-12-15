type ide = string;;

(*ambiente polimorfo*)
type 't env = ide -> 't;;
let emptyenv (v : 't) = function x -> v;;
let applyenv (r : 't env) (i : ide) = r i;;
let bind (r : 't env) (i : ide) (v : 't) = function x -> if x = i then v else applyenv r x;;

type exp = 
			Eint of int 
		| Ebool of bool 
		| Estring of string
		| Den of ide 
		| Prod of exp * exp 
		| Sum of exp * exp 
		| Diff of exp * exp 
		|	Eq of exp * exp 
		| Minus of exp 
		| IsZero of exp 
		| Or of exp * exp 
		| And of exp * exp 
		| Not of exp 
		|	Ifthenelse of exp * exp * exp 
		| Let of ide * exp * exp 
		| Fun of ide * exp 
		| FunCall of exp * exp 
		| Letrec of ide * exp * exp
		| For_all of exp * exp
		| Exist of exp * exp
		| Filter of exp * exp
		| Map of exp * exp
		
		(*Valori set*)
		| Set of exp list
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

    (*Insieme*)
    | SetVal of evT list
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
							|_ ->false)
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

let toexp (a : evT) = match a with
    | Int(q) -> Eint(q)
    | Bool(q) -> Ebool(q)
    | String(q) -> Estring(q)
    | _-> failwith("Convert error");;

(*interprete*)
let rec eval (e : exp) (r : evT env) : evT = match e with
			Eint n -> Int n 
		| Ebool b -> Bool b 
		| Estring(a) -> String(a)
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
		| Ifthenelse(a, b, c) -> 	let g = (eval a r) in
				if (typecheck "bool" g) 
					then (if g = Bool(true) then (eval b r) else (eval c r))
					else failwith ("nonboolean guard") 
		|	Let(i, e1, e2) -> eval e2 (bind r i (eval e1 r)) 
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
																			Fun(i, fBody) -> let r1 = (bind r f (RecFunVal(f, (i, fBody, r)))) in
																								eval letBody r1 
																		| _ -> failwith("non functional def"))
	

    (*=================== Estensione Interprete ====================*)
    (*=================== Ricorda il typecheck  ====================*)
    
    | Set(a) -> let rec evalset l r = match l with
                                        [] -> []
                                      | element::tail -> (eval element r)::(evalset tail r) in SetVal(evalset a r)

    | Insert (s, toAdd) -> (
                            match eval s r with
                              SetVal(l) -> if ((contains  toAdd l) = Bool(false) ) then SetVal((eval toAdd r)::l)
                                                else failwith("Already Existing")
                            | _-> failwith("Not a Set"))

    | IsIn (s, query) -> (
                          match eval s r with
                            SetVal(l) -> (contains query l)
      | _-> failwith("Not a Set"))
    
    | Rm (s, toDel) -> (
                        match eval s r with
                          SetVal(l) -> SetVal(delete toDel l)
                        | _-> failwith("Not a Set"))

  	| Getmin (s) -> (match eval s r with
                        SetVal(l) -> findmin l
											| _-> failwith("Not a Set"))
											
		| Getmax (s) ->( match eval s r with
											SetVal(l) -> findmax l
										| _-> failwith("Not a Set"))

		| IsEmpty (s) -> (match eval s r with
												SetVal(l) -> (match l with
																			| [] -> Bool(true)
																			| h::t -> Bool(false)
																			|_ -> failwith("Not a valid Set"))

												| _ -> failwith("Not a Set"))

		| IsSubset (s1, s2) -> (match (eval s1 r, eval s2 r) with
																(SetVal(l), SetVal(m)) -> Bool(sublist l m)
															| _ -> failwith("Not a valid enty")
															
															)
												


and subaux (l : evT list)(m : evT list) : bool = match l, m with
																								  [], _ -> true
																								| _, [] -> false
																								| hdl::tll, hdm::tlm -> hdl = hdm && subaux tll tlm
and sublist (l : evT list)(m : evT list) : bool = match l, m with
																									[], _ -> true
																									| _, [] -> false
																									| hdl::_, hdm::tlm -> hdl = hdm && subaux l m ||sublist l tlm													
and findmin (l : evT list) : evT = match l with
                        | [] -> Int(0)
                        | [element] -> element
												| element::tail -> let smallest = findmin tail in if element < smallest then element else smallest

and findmax (l : evT list) : evT = match l with
												| [] -> Int(0)
												| [element] -> element
												| element::tail -> let biggest = findmax tail in if element > biggest then element else biggest

and delete (toDelete : exp)(l : evT list) : (evT list) = match l with 
                                                            [] -> []
                                                          | element::tail -> if toDelete =  (toexp element) then (delete toDelete tail) 
                                                                              else element::(delete toDelete tail)
and contains (toSearch : exp)(l : evT list) : evT = match l with
                                                    [] -> Bool(false)
                                                  | element::tail -> if toSearch = (toexp element) then Bool(true) else contains toSearch tail;;
	






		
		
(* =============================  TESTS  ================= *)

(* basico: no let *)
let env0 = emptyenv Unbound;;

let set0 = Set([Eint(10);Eint(18);Eint(2)]);;
let s0 = eval set0 env0;;

let set1 = Insert(set0, Eint(3));;
let s1 = eval set1 env0;;

let set2 = IsIn(set0, Eint(1));;
let s2 = eval set2 env0;;

let min = Getmin(set0);;
let s2 = eval min env0;;

let max = Getmax(set0);;
let s3 = eval max env0;;

let set3 = Set([]);;
let s3 = eval set1 env0;;

let empty = IsEmpty(set1);;
let s4 = eval empty env0;;

let set4 = Insert(set3, Eint(18));;
let s5 = eval set4 env0;;

let sub = IsSubset(set4, set0);;
let s6 = eval sub env0;;



