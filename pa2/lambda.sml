structure Lambda: LAMBDA = 
struct

datatype t
	= Var of string
	| Abs of string * t
	| App of t * t
	
exception Todo

exception NoRule

(*generate fresh variables when called*)
val counter = ref 0
fun fresh () = 
	let val n = !counter
		val _ = counter := n + 1
		in concat ["x_", Int. toString n]
		end

(*judge if x is a value*)
fun isValue t =
	case t
	of Abs(string,_) => true 
	| _=> false

(*alpha  change name *)
fun alpha (oldname:string,newname:string, t) = (*unkown*)
	case t
	of Var x => if x = oldname then Var newname else Var x
	| Abs(x, t1) => if (x = oldname) 
					then Abs(newname , alpha(newname, oldname,t1))
					else Abs(x,alpha(newname, oldname,t1))
	| App(t1, t2) => App(alpha(newname,oldname,t1),alpha(newname,oldname,t2))

(*substitute x with s*)
fun substitute (x:string, s, t) =
	case t
	of Var y =>
		if y = x then s else Var y
	| Abs (y, t1) =>
		let val newname = fresh()
			val t2 = alpha(y,newname,t)
		in Abs(newname, substitute(x,s,t2))
		end
	| App(t1, t2) => App(substitute(x,s,t1),substitute(x,s,t2))
	
(*one-step evaluator*)
fun eval t = (*raise Todo*)
	case t
		(*of App(Abs(x,t12),v2) => if isValue v2
		then substitute(x,v2,t12)*)
		of App(t1, t2) =>
			case t1 
				of App(t1', t2') => App(eval t1, t2)
				| Abs(x,t12) =>
					if (isValue t2 )    (*E-AppAbs*)
					then substitute(x,t2,t12)
					else App( Abs(x,t12), eval t2) 
				| _=> raise NoRule
	
fun pp t =
    case t
     of Var x => print x
      | Abs (x, e) => 
        (print "\\lambda "; print x; print ".("
       ; pp e; print ")")
      | App (e1, e2) =>
        (print "("; pp e1; print ") "; print "("
       ; pp e2; print ")")
	   
fun evalAll t =
    (let val t' = (eval t)
         val _ = pp t'
         val _ = print "\n"
     in evalAll t'
     end) handle NoRule => t 

end (* structure Lambda *)		

(* a union test*)

val omega = Lambda.Abs("x",Lambda.App(Lambda.Var "x",Lambda.Var "x"))
		
val Omega = Lambda.App(omega,omega)

val _= (Lambda.pp omega; print "\n")
 
val _= (Lambda.pp (Lambda.eval Omega); print "\n") 
				
