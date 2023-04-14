{
  open Parser

  let keywords = Hashtbl.create 97
  let () = List.iter (fun (s,t) -> Hashtbl.add keywords s t)
  ["if", IF; "then", THEN; "else", ELSE;
   "while", WHILE;
   "skip", SKIP;
   "return", RET;
   "act", ACT;
   "get", GET;
   "call", CALL;
   "true", TRUE; "false", FALSE;
   "True", TRUE; "False", FALSE;
   "TRUE", TRUE; "FALSE", FALSE;
   "undefined", UNDEFINED;
   "service", SERVICE;
   "client", CLIENT;
   "not_implemented", NOTIMPLEMENTED;
   "fi", FI;
    ]


  exception Lexing_error of string

  let transform s =
    let rec aux i s' =
      if i = String.length s then s' else
        begin
          if s.[i] = '\\' then
            match s.[i+1] with
            |'n' -> aux (i+2) (s' ^ String.make 1 '\n')
            |'t' -> aux (i+2) (s' ^ String.make 1 '\t')
            |'"' -> aux (i+2) (s' ^ String.make 1 '\"')
            |'\\' -> aux (i+2) (s' ^ String.make 1 '\\')
            |_ -> raise (Lexing_error "string not valid")
          else
            aux (i+1) (s' ^ String.make 1 s.[i])
        end in
    aux 0 ""

}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z' '_']
let var = letter (letter | digit | '-')*
let number = digit | digit (digit | '_')* digit
let char = ['\032'-'\033' '\035'-'\091' '\093'-'\126'] | '\\''\\' | '\\''"' | "\\n" | "\\t" (*ascii*)
let string = '"' char* '"'

rule token = parse
| [' ' '\t']+ {token lexbuf}
| "--" {one_line_comment lexbuf}
| "(-" {comment lexbuf; token lexbuf}
| '\n' {Lexing.new_line lexbuf ; token lexbuf}
| var as v {try Hashtbl.find keywords v with Not_found -> IDENT v}
| number as n {let n = int_of_string n in NUM n}
| string as s {let s = transform (String.sub s 1 (String.length s - 2)) in STR s}
| eof {EOF}
| "+" {PLUS}
| "-" {MINUS}
| "*" {TIMES}
| "<=" {LEQ}
| ">=" {GEQ}
| "=" {EQ}
| "&" {AND}
| "|" {OR}
| "^" {XOR}
| "!" {NOT}
| "~" {TILDE}
| "$" {DOLLAR}
| ";" {SEMICOLON}
| "," {COMMA}
| "(" {LPAR}
| ")" {RPAR}
| "{" {LPB}
| "}" {RPB}
| ":=" {ASS}
| "::" {SRVS}
| _ as s {raise (Lexing_error ("Illegal input : " ^ String.make 1 s))}

and comment = parse
| "-)" {()}
| "(-" {comment lexbuf ; comment lexbuf}
| eof {raise (Lexing_error "comment not ended")}
| "\n" {Lexing.new_line lexbuf ; comment lexbuf}
| _ {comment lexbuf}

and one_line_comment = parse
| "\n" {Lexing.new_line lexbuf ; token lexbuf}
| eof {EOF}
| _ {one_line_comment lexbuf}
