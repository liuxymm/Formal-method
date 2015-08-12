structure Reference: REFERENCE =
struct

structure Type =
struct
datatype t
  = Bool
  | Unit
  | Fun of t * t  (*parameter is t1  result is t2*)
  | Ref of t
           
fun equals (t1, t2) =
    case (t1, t2)
     of (Bool, Bool) => true
      | (Fun (s1, s2), Fun (s3, s4)) =>
        equals (s1, s3) andalso
        equals (s2, s4)
      | (Ref s1, Ref s2) =>
        equals (s1, s2)
      | (Unit, Unit) => true
      | _ => false
             
fun toString t =
    case t
     of Bool => "bool"
      | Unit => "unit"
      | Fun (t1, t2) =>
        String.concat [toString t1
                     , " -> "
                     , toString t2]
      | Ref t =>
        String.concat [toString t
                     , " ref"]
exception TypeError
end (* structure Type *)

(* we extend the language of lambda-calculus
 * with reference, as we discussed in class: there
 * are 3 reference-related operations:
 *   1. create a fresh reference with initial value e: ref e
 *   2. read the value pointed by a reference e: !e, and
 *   3. update the address pointed by e1 with the value of e2: e1 := e2.
 *)
datatype t
  = True
  | False
  | If of t * t * t
  | Var of string
  | Abs of string * Type.t * t
  | App of t * t
  | Ref of t          (* ref e *)
  | Deref of t        (* !e *)
  | Assign of t * t   (* e1 := e2 *)
  | Address of string (* l *)
  | Unit
exception BadAddress        
exception NoRule

structure Heap =
struct

val counter = ref 0
fun fresh () =
    let val n = !counter
        val _ = counter := !counter + 1
    in  String.concat ["x_", Int.toString n]
    end
    
type heap = (string -> t) ref
type typing = (string -> Type.t) ref

val heap: heap = ref (fn _ => raise BadAddress)
val types: typing = ref (fn _ => raise BadAddress)

fun alloc (t) = 
    let val newAddress = fresh ()
        val _ = heap := (fn y =>
                           if y = newAddress
                           then t
                           else (!heap) y)
    in  newAddress
    end

fun lookup (x) = (!heap) x

fun getType (x) = (!types)x 

fun update (x, t) = 
    heap := (fn y =>
                if y=x
                then t
                else (!heap) y)
		

end (* structure Heap *)

fun pp t =
    case t
     of Ref t => (print "ref "; pp t)
      | Deref t => (print "!"; pp t)
      | Assign (t1, t2) =>
        (pp t1; print " := "; pp t2)
      | Address x => print x
      | Unit => print "()"
	  | Abs(x,ty,t2) => (print"\\lambda "; print x; print ":";
						print (Type.toString ty); print "(";
						pp t; print ")")
	  | App(t1,t2) => (pp t1;print "->" ;pp t2)
	  | Var x => print x
	  | True => (print "True")
	  | False => (print "False")
	  | If(t1,t2,t3) => (print "If(";
						pp t1;
						print ")";
						print "then(";
						pp t2;
						print ")";
						print "else(";
						pp t3;
						print ")")

fun isValue (t) = case t 
	of Abs(string,_,_) => true
	| True => true
	| False => true
	| Unit => true
	| Address _ => true
	| _ => false
	
(*substitute x with v apply to t*)
fun substitute (x,v,t) = 
	case t 
	of Var x1 => 
		if x1 = x 
		then v
		else t
	| Abs(x,ty,t1) => Abs(x,ty, substitute(x,v,t1))
	| App(t1,t2) => App(substitute(x,v,t1),substitute(x,v,t2))
	| Ref(t) => Ref(substitute(x,v,t))
	| Deref(t) => Deref(substitute(x,v,t))
	| Assign(t1,t2) => Assign(substitute(x,v,t1),substitute(x,v,t2))
	| _ => t
    
(* #########################################*)
(* Your first job is to finish this function. *)
fun Check (env, t) = case t of
	Unit => Type.Unit
	| True => Type.Bool
	| False => Type.Bool
	| If(t1,t2,t3) =>       (*T-If*)
		let val ty1 = Check(env,t1)
		in if (ty1 = Type.Bool )  
			then let val ty2 = Check(env,t2)
			         val ty3 = Check(env,t3)
				in if (ty2 = ty3)
					then ty2
					else raise Type.TypeError
				end
			else raise Type.TypeError
		end
	| Var x => env x (*T-Var*)
	| Abs(x,ty,t2) => 
		let val ty2=Check(fn y=> if x = y then ty else env y,t2)
			in Type.Fun(ty,ty2)
			end (*T-Abs*)
	| App(t1, t2) =>
		let val ty2 = Check(env, t2)
			val ty1 = Check(env,t1)
		in case ty1 of
			(*(case Check(env, t1) of*)
				Type.Fun(ty1,ty3) =>
					if Type.equals(ty1,ty2)
					then ty3
					else raise Type.TypeError
				| _ => (*let val _ = pp t2
						in Type.Unit
						end (*T-App*)*)
						raise Type.TypeError
		end
	| Ref t => let val ty = Check (env, t)
				in Type.Ref ty
				end 
		
	| Deref t => 
		let val ty1 = Check(env,t)
		in case ty1 of 
			Type.Ref t1 => t1
			| _ => raise Type.TypeError
		end 
	| Address l  => Type.Ref(Heap.getType l )
	| Assign (t1,t2) => 
		let val ty1 = Check(env, t1)
			val ty2 = Check(env, t2)
		in case ty1 of
			Type.Ref ty3 => if(Type.equals(ty2,ty3))
							then Type.Unit
							else raise Type.TypeError
			| _ => raise Type.TypeError
		end
	(*raise Todo*)

fun typeCheck(t:t):Type.t = Check(
	fn _ => raise Type.TypeError, t)

(* to simplify the interface of the eval
 * function, we can make the heap global, 
 * instead of an argument to this function.
 *)




(*exception Todo*)

(* #########################################*)
(* Your second job is to finish this function. *)
fun eval t = 
	case t 
	of If(True,t1 , _) => t1
	| If(False, _, t1) => t1
	| App(t1,t2) => 
		if isValue(t1)
		then case t1 
			of Abs(x,ty,t1') => 
				if isValue(t2) 
				then substitute(x,t2,t1')  (*E-AppAbs*)
				else App(t1, eval t2) (*E-App2*)
			| _ => raise NoRule
		else App(eval t1, t2)   (*E-App1*)
	| Ref t1 =>
		if isValue (t1) 
		then Address(Heap.alloc(t1)) (*E-RefV*)
		else Ref (eval t1)  (*E-Ref*)
	| Deref t1 =>
		(case t1 
		of Address l => Heap.lookup(l)
		| _ => Deref(eval t1) )(*E-Deref*)
	| Assign(t1,t2) => 
		if isValue t1
		then if isValue t2
			then case t1 of 
				Address (l) => 
					let val _ = Heap.update(l, t2)
					in Unit
					end 
				| _ => raise NoRule
			else Assign(t1,eval t2)
		else Assign(eval t1, t2)
	| _=> raise NoRule
	
(*raise Todo*)


     

fun evalAll t =
    (let val t' = (eval t)
         val _ = pp t'
         val _ = print "\n"
     in evalAll t'
     end) handle NoRule => t 

(* unit test *)

val t1 = Ref Unit
val ty1 = typeCheck (t1)
val _ = (print (Type.toString ty1); print "\n")

val t2 = Abs ("x", Type.Ref Type.Unit,True)

val ty2 = typeCheck t2
val _ = (print (Type.toString ty2);print "\n")

val t3 = App (t2, t1)
val ty3 = typeCheck t3
val _ = (print (Type.toString ty3);print "\n")

val _ = print "t4\n\n\n"
val t4 = Assign(Ref Unit, Deref (Ref Unit))
val _ = evalAll t4

end (* structure Reference *)



