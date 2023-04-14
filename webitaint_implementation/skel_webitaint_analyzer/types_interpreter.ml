module Lst = List
open Monads
open Wtypes


exception Fail of string

module MT = Monads.Rand(Monads.ID)
module SMap = Map.Make(String)

module rec T_Types : sig
         type out_chan = out_channel
         type 'a _list = 'a Lst.t
         type nat = int
         type 'a state = 'a SMap.t
         type str = string
end = T_Types

module T_Input = struct
  include Unspec(MT)(T_Types)

  let std_out = stdout
  let flush_out = flush
  let lmk_empty = []
  let zero = 0
  let one = 1
  let minus_one = -1
  let _unit = "value : unit\n"
  let _undefined = "value : undefined\n"
  let _ass = "statement evaluation : assignment\n"
  let _ret = "statement evaluation : return\n"
  let _seq = "statement evaluation : sequence (s1',s2)\n"
  let _cond = "statement evaluation : conditional\n"
  let _while = "statement evaluation : while\n"
  let _skip = "skip"
  let _not_implemented = "extended value : Not_implemented\n"
  let _print s = MT.ret (print_string s; print_string "\n"; flush stdout)
  let str_concat (s,ls) = MT.ret (String.concat s ls)
  let str_of_nat n = MT.ret (string_of_int n)
  let str_of_bool b = MT.ret (string_of_bool (match b with | T -> true | F -> false))
  let add (x,y) = MT.ret (x + y)
  let eq (x,y) = MT.ret (match x = y with | true -> T | false -> F)
  let geq (x,y) = MT.ret (match x >= y with | true -> T | false -> F)
  let init_state (():unit) : ('a state) MT.t = MT.ret SMap.empty
  let ladd (v,l) = MT.ret ([v]@l)
  let ladd_end (l,v) = MT.ret (l@[v])
  let lappend (l1,l2) = MT.ret (l1@l2)
  let lassoc i =
    MT.ret (fun l ->
        let v = Lst.assoc i l in
        let l' = Lst.remove_assoc i l in
        MT.ret (v,l'))
  let lassoc_opt i =
    MT.ret ( fun l ->
             let o = Lst.assoc_opt i l in
             match o with
             | Some v ->  let l' = Lst.remove_assoc i l in
                          MT.ret (VSome (v,l'))
             | None -> MT.ret VNone)
  let lcombine li = MT.ret (fun lv -> MT.ret (Lst.combine li lv))
  let lempty l = MT.ret (match l with | [] -> T | _ -> F)
  let leq (x,y) = MT.ret (match x <= y with | true -> T | false -> F)
  let lfold_l f = MT.ret ( fun v ->
                           MT.ret (fun l ->
                               MT.ret (Lst.fold_left (fun v' -> fun l' -> MT.extract (MT.extract (f v') l'))
                                         v
                                         l)))
  let lhd l = (match l with | [] -> MT.fail "empty list. no head" | x::t -> MT.ret (x,t))
  let llength l = MT.ret (Lst.length l)
  let lmap f = MT.ret (fun l ->  MT.ret (Lst.map (fun v' -> MT.extract (f v')) l))
  let lpartition f=
    MT.ret (fun l -> let f' = (fun (v : 'a) -> match MT.extract (f v) with | T -> true | F -> false) in MT.ret (Lst.partition f' l))
  let lpick l =
    let l_l = (Lst.length l) in
    Random.self_init ();
    let i = Random.int (MT.extract (match l_l with
                        | 0 -> MT.fail "empty list"
                        | i -> MT.ret i)) in
    let v = Lst.nth_opt l i in
    match v with
    | None -> MT.fail "cannot pick an element"
    | Some v -> MT.ret (v,Lst.filter (fun (v':'a) -> v' <> v ) l)
  let lremove_assoc i =
    MT.ret (fun l -> let v = Lst.assoc_opt i l in
                     match v with
                     | None -> MT.fail "cannot remove the element from the list"
                     | Some v -> let l' = Lst.remove_assoc i l in MT.ret ((i,v),l'))
  let lrev l = MT.ret (Lst.rev l)
  let lsplit l = MT.ret (Lst.split l)
  let mul (x,y) = MT.ret (x * y)
  let str_eq (s1,s2) = MT.ret (match s1 = s2 with | true -> T | false -> F)
  let sub (x,y) = MT.ret (x - y)
  let st_read (x : var)  =
    MT.ret (fun (s : 'a state) -> let v = SMap.find_opt x s in
                                  match v with
                                  | Some v -> MT.ret (v,s)
                                  | None -> MT.fail (String.concat "" [x;" not present in the map."]))
  let st_write x =
    MT.ret (fun v -> MT.ret (fun (s: 'a state) -> let s' = SMap.add x v s in MT.ret ((),s')))
  let _plus = "+"
  let _minus = "-"
  let _times = "*"
  let _xor = "xor"
  let _or = "||"
  let _and = "&"
  let _neg = "!"
  let _leq = "<="
  let _geq = ">="
  let _eq = "="
  let _OK = "OK"
  let _ERROR = "ERROR"
  let str_of_bop o = MT.ret (fun e1 -> MT.ret (fun e2 -> MT.ret (String.concat " " [e1;o;e2])))
  let str_of_uop o = MT.ret (fun e -> MT.ret (String.concat " " [o;e]))
  let str_of_assignment x = MT.ret (fun e -> MT.ret (String.concat "" [x;":=";e]))
  let str_of_sequence s1 = MT.ret (fun s2 -> MT.ret (String.concat "; \n" [s1;s2]))
  let str_of_cond e = MT.ret (fun s1 -> MT.ret (fun s2 -> MT.ret (String.concat "" ["if";"(";e;") then\n\t";s1;"\n else\n\t";s2;"\n fi"])))
  let str_of_while e = MT.ret (fun s -> MT.ret (String.concat "" ["while";"(";e;"){\n\t";s;"\n}"]))
  let str_of_ret e = MT.ret (String.concat " " ["return";e])
  let str_of_output_v v = MT.ret (String.concat " " ["VALUE";v])
  let str_of_output_s s =  MT.ret (fun ack -> MT.ret (String.concat "" ["STMT ";s;"\n with ack ->\n";ack]))
  let pp_deviceID c = MT.ret (fun d -> MT.ret (let (name,location) = d in Printf.fprintf c "device_id = (%s,%s)\n" name location))

end

module T_Interp = MakeInterpreter(T_Input)
open T_Interp
