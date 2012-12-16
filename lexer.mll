{
  open Parser

  

}


let newline = ('\r' '\n' | '\n' '\r' | '\r' | '\n' )
let var = ['a'-'z''A'-'Z''0'-'9']+

rule token = parse 
  | newline          { Lexing.new_line lexbuf ; token lexbuf }
  | " "+             { token lexbuf }
  | "\\/"            { FORALL }
  | "\\"             { LAM }
  | "fun"            { FUN }
  | "!"              { OF_COURSE }
  | "("              { LPAR }
  | ")"              { RPAR }
  | "["              { LSQBR }
  | "]"              { RSQBR }
  | "<"              { LT }
  | ">"              { GT }
  | "="              { EQ }
  | "*"              { STAR }
  | "->"             { ARR }
  | "-o"             { LARR }
  | "."              { DOT }
  | ":"              { COLON }
  | ","              { COMMA }
  | "destruct"       { DESTRUCT }
  | "as"             { AS }
  | "in"             { IN }
  | "type"           { TYPE }
  | "let"            { LET }
  | var as s         { VAR(s) }
  | eof              { EOF }
