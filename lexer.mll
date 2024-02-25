(* TODO: Complete the lexical specification for the S language. *)

{
 open Parser
 exception Eof
 exception LexicalError
 let comment_depth = ref 0
} 

let blank = [' ' '\n' '\t' '\r']+
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let number = ['0'-'9']+

rule start = parse
  | blank { start lexbuf }
  | "/*" { comment_depth :=1; comment lexbuf; start lexbuf }
  | eof   { EOF}
  | number (*as number*) { (*print_endline ("NUM: "^number);*) NUM (int_of_string (Lexing.lexeme lexbuf))}
  | "int" { (*print_endline "INT";*) INT }
  | '+' { (*print_endline "PLUS";*) PLUS }
  | '-' { (*print_endline "MINUS";*) MINUS }
  | '*' { (*print_endline "STAR";*) STAR }
  | '/' { (*print_endline "SLASH";*) SLASH }
  | '=' { (*print_endline "EQUAL";*) EQUAL }
  | "==" { (*print_endline "EQUALEQUAL";*) EQUALEQUAL }
  | "<=" { (*print_endline "LE";*) LE }
  | "<" { (*print_endline "LT";*) LT }
  | ">=" { (*print_endline "GE";*) GE }
  | ">" { (*print_endline "GT";*) GT }
  | "!" { (*print_endline "NOT";*) NOT }
  | "&&" { (*print_endline "AND";*) AND }
  | "||" { (*print_endline "OR";*) OR }
  | "if" { (*print_endline "IF";*) IF }
  | "else" { (*print_endline "ELSE";*) ELSE }
  | "while" { (*print_endline "WHILE";*) WHILE }
  | "do" { (*print_endline "DO";*) DO }
  | "read" { (*print_endline "READ";*) READ }
  | "print" { (*print_endline "PRINT";*) PRINT }
  | ';' { (*print_endline "SEMICOLON";*) SEMICOLON }
  | '{' { (*print_endline "LBRACE";*) LBRACE }
  | '}' { (*print_endline "RBRACE";*) RBRACE }
  | '[' { (*print_endline "LBLOCK";*) LBLOCK }
  | ']' { (*print_endline "RBLOCK";*) RBLOCK }
  | '(' { (*print_endline "LPAREN";*) LPAREN }
  | ')' { (*print_endline "RPAREN";*) RPAREN }
  | id (*as id*){ (*print_endline ("ID: "^id);*) ID (Lexing.lexeme lexbuf) }
  | _ { raise LexicalError }

and comment = parse
  | "/*" {comment_depth := !comment_depth+1; comment lexbuf}
  | "*/" {comment_depth := !comment_depth-1;
          if !comment_depth > 0 then comment lexbuf }
  | eof {raise Eof}
  | _   {comment lexbuf}