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
  | "{"              { LBR }
  | "}"              { RBR }
  | "["              { LSQBR }
  | "]"              { RSQBR }
  | "<"              { LT }
  | ">"              { GT }
  | "="              { EQ }
  | "+"              { PLUS }
  | "*"              { STAR }
  | "->"             { ARR }
  | "-o"             { LARR }
  | "."              { DOT }
  | ":"              { COLON }
  | ";"              { SEMICOLON }
  | ","              { COMMA }
  | "destruct"       { DESTRUCT }
  | "match"          { MATCH }
  | "as"             { AS }
  | "in"             { IN }
  | "type"           { TYPE }
  | "let"            { LET }
  | var as s         { VAR(s) }
  | eof              { EOF }
