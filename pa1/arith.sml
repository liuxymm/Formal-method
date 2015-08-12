structure Arith: ARITH =
struct

datatype t 
  = True
  | False
  | If of t * t * t
  | Zero
  | Succ of t
  | Pred of t
  | IsZero of t

exception AddYourCodeHere
          
fun isNumber t =
    case t
     of Zero => true
      | Succ t' => isNumber t'
      | _ => false
             
exception NoRule
          
(* one-step evaluator *)
fun eval t =
    case t
     of If (True, t2, _) => t2
      | If (False, _, t3) => t3
      | If (t1, t2, t3) =>
        If (eval t1, t2, t3)
      | Succ t' => Succ (eval t')
      | Pred Zero => Zero
      | Pred (Succ t') =>
        if isNumber t'
        then t'
        else Pred (eval (Succ t'))
      | Pred t' => Pred (eval t')
      | IsZero Zero => True
      | IsZero (Succ t') =>
        if isNumber t'
        then False
        else IsZero (eval (Succ t'))
      | IsZero t' => IsZero (eval t')
      | _ => raise NoRule

fun pp t =
    case t
     of True => print "True"
      | False => print "False"
      | If (t1, t2, t3) =>
        (print "If("
       ; pp t1; print ", "; pp t2; print ", "
       ; pp t3; print ")")
      | Zero => print "Zero"
      | Succ t' =>
        (print "Succ("; pp t'; print ")")
      | Pred t' =>
        (print "Pred("; pp t'; print ")")
      | IsZero t' => 
        (print "IsZero("; pp t'; print ")")

fun evalAll t =
    (let val t' = (eval t)
         val _ = pp t'
         val _ = print "\n"
     in evalAll t'
     end) handle NoRule => t 

(* Your job: *)
fun evalBig t = 
	case t
	of True => True                             (*B-value*)
	| False => False
	| Zero => Zero 
	| If(t1, t2, t3)=>                           (*B-IFTRUE FALSE*)
		let val t' = evalBig t1
		in (if t' = True
			then evalBig t2
			else evalBig t3 (*case t'*)
			)
		end
	| Succ t1 =>                                       (*B-SUCC*)
	   let val t' = evalBig t1
	   in if isNumber t'
		  then Succ t'
		  else Succ (evalBig t')
		end
		  
	| Pred t1 => 										(*B-PREDZERO*)
		let val t' = evalBig t1
		in case t'
		of Zero => Zero
		| Succ t2 =>
		   if isNumber t2
		   then t2
		   else Succ (evalBig t2)
		end   
	| IsZero t1 =>                                      (*B-IsZero Succ*)
		let val t' = evalBig t1
		in case t'
			of Zero => True
			| Succ t2 =>
				if isNumber t2
				then False 
				else evalBig(Succ t2) 
				
		end
 
	
end (* structure Arith *)

(* a unit test *)
val e = Arith.Pred (Arith.Succ 
                        (Arith.Pred Arith.Zero))

val _ = (Arith.pp e; print "\n")

val _ = Arith.evalAll e
        
val _ = Arith.evalBig e
val _ = (print "evalBig\n";Arith.pp (Arith.evalBig e ); print "\n")

(*use case*)
val e1 = Arith.True
val e2 = Arith.False
val e3 = Arith.Zero

val e4 = Arith.If (Arith.True,Arith.Pred(Arith.Succ(Arith.Zero)),Arith.Zero)
val e5 = Arith.If (Arith.False,Arith.Zero,Arith.Pred(Arith.Succ(Arith.Zero))) (*B-IFTRUE FALSE*)
val e6 = Arith.Succ(Arith.Succ (Arith.Succ Arith.Zero)) (*B-SUCC*)
val e7 = Arith.Pred(Arith.Pred (Arith.Pred Arith.Zero))   (*B-PREDZERO*)
val e8 = Arith.Pred (Arith.Succ(Arith.Pred Arith.Zero))  (*B_PREDSUCC*)

val e9 = Arith.IsZero(Arith.Pred Arith.Zero) (*B-IsZeroZero*)
val e10 = Arith.IsZero(Arith.Succ(Arith.Pred Arith.Zero)) (*B-IsZeroSUCC*)

val _ = print "B-VALUE\n" 
val _ = print " one-by-one \n"
val _ = Arith.pp e1
val _ = print "\n"
val _ = Arith.evalAll e1
val _ = print " big-step"
val _ = Arith.evalBig e1
val _ = (print "\n"; Arith.pp (Arith.evalBig e1); print "\n";print "\n")

val _ = print " one-by-one \n"
val _ = Arith.pp e2
val _ = print "\n"
val _ = Arith.evalAll e2
val _ = print " big-step"
val _ = Arith.evalBig e2
val _ = (print "\n"; Arith.pp (Arith.evalBig e2);print "\n";print "\n")

val _ = print " one-by-one \n"
val _ = Arith.pp e3
val _ = print "\n"
val _ = Arith.evalAll e3
val _ = print " big-step"
val _ = Arith.evalBig e3
val _ = (print "\n"; Arith.pp (Arith.evalBig e3))
val _ = (print "\n";print "\n")

val _ = print "B-IFTRUE\n"   
val _ = print " one-by-one \n"
val _ = Arith.pp e4
val _ = print "\n"
val _ = Arith.evalAll e4
val _ = print " big-step"
val _ = (print "\n"; Arith.pp (Arith.evalBig e4))
val _ = (print "\n";print "\n")

val _ = print "B-IFFALSE\n"   
val _ = (print " one-by-one"; print "\n")
val _ = Arith.pp e5
val _ = print "\n"
val _ = Arith.evalAll e5
val _ = print " big-step"
val _ = Arith.evalBig e5
val _ = (print "\n"; Arith.pp (Arith.evalBig e5))
val _ = (print "\n";print "\n")

val _ = print "B-SUCC\n"   
val _ = print " one-by-one \n"
val _ = Arith.pp e6
val _ = print "\n"
val _ = Arith.evalAll e6
val _ = print " big-step"
val _ = Arith.evalBig e6
val _ = (print "\n"; Arith.pp (Arith.evalBig e6))
val _ = (print "\n";print "\n")

val _ = print "B-PREDZERO\n"   
val _ = print " one-by-one \n"
val _ = Arith.pp e7
val _ = print "\n"
val _ = Arith.evalAll e7
val _ = print "big-step "
val _ = (print "\n"; Arith.pp (Arith.evalBig e7))
val _ = (print "\n";print "\n")

val _ = print "B-PREDSUCC\n"   
val _ = print " one-by-one \n"
val _ = Arith.pp e8
val _ = print "\n"
val _ = Arith.evalAll e8
val _ = print "big-step"
val _ = Arith.evalBig e8
val _ = (print "\n"; Arith.pp (Arith.evalBig e8))
val _ = (print "\n";print "\n")

val _ = print "B-ISZEROZERO\n"   
val _ = print " one-by-one \n"
val _ = Arith.pp e9
val _ = print "\n"
val _ = Arith.evalAll e9
val _ = print "big-step"
val _ = Arith.evalBig e9
val _ = (print "\n"; Arith.pp (Arith.evalBig e9))
val _ = (print "\n";print "\n")

val _ = print "B-ISZEROSUCC\n"   
val _ = print " one-by-one \n"
val _ = Arith.pp e10
val _ = print "\n"
val _ = Arith.evalAll e10
val _ = print "big-step"
val _ = Arith.evalBig e10
val _ = (print "\n"; Arith.pp (Arith.evalBig e10))




