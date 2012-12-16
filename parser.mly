%{

  open LinearType
  open SimplyTypedLambda

  let term_symbol_table = Hashtbl.create 173
  let type_symbol_table = Hashtbl.create 173

  let get_result () = 
    let f x y acc = (x,y) :: acc in 
    let g t = Hashtbl.fold f t [] in
    (g term_symbol_table, g type_symbol_table)

%}


%token EOF COLON COMMA 
%token<string> VAR
%token STAR LARR OF_COURSE FORALL TYPE
%token LAM FUN LPAR RPAR LT GT EQ ARR DOT LET LSQBR RSQBR DESTRUCT AS IN

%nonassoc VAR LPAR RPAR EOF LAM

%right LARR STAR
%nonassoc OF_COURSE


%start<LinearType.linear_type> linear_type_init
%start<SimplyTypedLambda.typed_lambda> term_init
%start<unit> type_def_init
%start<unit> term_def_init
%start<(string * SimplyTypedLambda.typed_lambda) list * (string * LinearType.linear_type) list> file 


%%

linear_type :
| s = VAR          { base s }
| OF_COURSE t = linear_type      { ! t }
| t1 = linear_type STAR t2 = linear_type  { times t1 t2 }
| t1 = linear_type LARR t2 = linear_type  { t1 => t2 }
| LPAR t = linear_type RPAR               { t }
| LSQBR v = VAR RSQBR  { Hashtbl.find type_symbol_table v }

type_def :
| TYPE v = VAR EQ t = linear_type  { Hashtbl.add type_symbol_table v t }

type_def_init :
| type_def EOF { () }

linear_type_init :
| t = linear_type EOF { t }


typed_var :
| s = VAR COLON t = linear_type { (s, t) }
| LPAR s = VAR COLON t = linear_type RPAR { (s, t) }


term_without_app :
| s = VAR         { var s }
| LSQBR v = VAR RSQBR  { Hashtbl.find term_symbol_table v }
| LAM args = typed_var+ DOT t = term { lam args t }
| FUN args = typed_var+ ARR t = term { lam (List.map (fun (x,y) -> (x,!y)) args) t }
| LPAR t = term RPAR   { t }

term :
| t0 = term args = term_without_app+  { apps t0 args }
| t = term_without_app   { t }


term_def : 
| LET v = VAR EQ t = term   { Hashtbl.add term_symbol_table v t }

term_def_init :
| term_def EOF { () }

term_init :
| t = term EOF { t }

defs :
| term_def { () }
| type_def { () }

file :
| defs+ EOF { get_result () }  








