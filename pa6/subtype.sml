structure Subtype: SUBTYPE =
struct

structure Type =
struct
datatype t
  = Record of (string*t) list
  | Top
  | String
  | Arrow of t * t
  
fun pp t =
 case t 
     of Record l =>
        let fun ppfields l =
                case l
                 of [] => ()
                  | (name, t)::xs =>
                    (print name
                   ; print ":"
                   ; pp t
                   ; print "; "
                   ; ppfields xs)
        in
            (print "{"
           ; ppfields l
           ; print "}")
        end
	  | Top=>print "Top"
	  | String=>print "String"
	  | Arrow (t1,t2)=>
			(print "("
			; pp t1
			; print ") ("
			; pp t2
			; print ")")
			

fun subTypeCheck (t1, t2) =
    case (t1, t2)
     of (S, Top) =>(*SA-TOP  S<:T*)
        true
	  | (String,String)=>true
      | (Arrow (s1, s2), Arrow (T1, T2)) =>(*SA-ARROW*)
        subTypeCheck (T1, s1) andalso
        subTypeCheck (s2, T2)
      | (Record [],Record []) =>true
      | (Record [],Record l)=> false
	  | (Record k,Record [])=>true
	  | (Record m,Record n)=>
	    let fun fields k l =
		  case l
		    of []=>true
			 | (lj, Sj)::xs=>
			       (let fun fie k  =
							case k
								of []=>false
								| (kj, Ti)::ls =>
								if (lj=kj) andalso subTypeCheck(Sj,Ti)
								then fields k xs
								else fie ls
					in fie k
					end)
		in fields m n
        end
      | _ => false
	  
	  
		
end (* structure Type *)

datatype t
  = Record of (string * t) list
  | Proj of t * string
  | String of string
  | Var of string
  | Abs of string * Type.t * t
  | App of t * t


exception Todo
exception TypeError

fun check (env, t) = (*raise Todo*)
	case t
		of Var x=>(*TA-VAR*)
		    env x  
	   	| String x=>Type.String
		| Abs (x,ty,t2)=>(*TA-ABS*)
		   Type.Arrow(ty,check (fn y => if x=y
                       then ty
                       else env y
             , t2))
		
		| App (t1,t2)=>(*TA-APP*)
			 (let val T1=check(env,t1) 
	           in case T1
	             of Type.Arrow(T11,T12)=>
		           let val T2=check(env,t2) 
		           in if(Type.subTypeCheck (T2,T11))
		              then T12
			          else raise TypeError 
		           end
			    | _ =>raise TypeError
	         end)
		 | Record l=>(*TA-RCD*)
		   let fun fields l =
                case l
                 of [] => []
                  | (name, t)::xs =>
				   let val T=check (env,t)
				   in  (name,T)::(fields xs)
				   end
            in Type.Record(fields l)
            end
		 | Proj (ls,x)=>
			(let val T=check(env,ls)
			in case T
			 of Type.Record r=>
				let fun field l x=(*l of string * t*)
					case l
					of [] => raise TypeError
					| (y, t1)::xs =>
						if x=y
						then Type.Record[(y,t1)]
						else field xs x    
					
				in
					field r x
				end
		   end)
		 
		 

fun typeCheck t = check (fn x => raise TypeError, t)




fun pp t =
    case t 
     of Record l =>
        let fun ppfields l =
                case l
                 of [] => ()
                  | (name, t)::xs =>
                    (print name
                   ; print " = "
                   ; pp t
                   ; print "; "
                   ; ppfields xs)
        in
            (print "{"
           ; ppfields l
           ; print "}")
        end
      | Proj (t, name) =>
        (pp t
       ; print "."
       ; print name)
      | String s =>
        print s
      | Var x =>
        print x
      | Abs (x, ty, t) =>
        (print "\\lambda "
       ; print x
       ; print ": "
       ; Type.pp ty
       ; print "."
       ; pp t)
      | App (t1, t2) =>
        (print "("
       ; pp t1
       ; print ") ("
       ; pp t2
       ; print ")")



(* unit test *)
val t = App (Abs ("x"
                , Type.Record [("name"
                              , Type.String)]
                , Proj (Var "x"
                      , "name"))
           , Record [("age", String "20")
                   , ("name", String "Bob")])
val _ = (pp t; print "\n")
val ty1 = typeCheck t
val _ = (Type.pp ty1; print "\n"; print "\n")




end
