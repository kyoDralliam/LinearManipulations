LinearManipulations
===================

About linear types and lambda calculus...

Currently, a simple lambda calculus with the constructions designed 
to be typed by the IMELL fragment of linear logic 
(intuitionnistic multiplicative exponential linear logic).
You can build simple terms from the combinators obtained from
the module SimplyTypedLambda and check its type with module SimplyTyping.
See module Print to print types and terms.

Requirements : 
- OCaml (>= 4.00.0) (should work with OCaml >= 3.12)
- menhir 

Build : 
From root directory : 

      $ make

or

        $ make cmo


if you prefer toying with the toplevel.


Future works / Projects :
- add a decent lexer/parser (partially done)
- add additives (only plus in fact) (done)
- add propositional linear types inferrence
- add second order and/or higher order types
- add effects 
- investigate about differential lambda-calculus
  and co-structural rules



Syntax (examples in test_nat) :

type_variable ::= A, B, C, ...

linear_type ::=   type_variable
                | linear_type + linear_type 
                | linear_type * linear_type 
                | linear_type -> linear_type
                | ! linear_type 
                | ( linear_type )
                | [def_var]

var ::= x, y, z, ...

typed_pair ::=   var : linear_type
               | ( var : linear_type )

term ::=   var
         | term term+
         | \ typed_pair+ . term
         | < term ; term >
         | destruct term as (var, var) in term
         | { term : linear_type | linear_type }
         | { linear_type | term : linear_type }
         | match term as | var -> term | var -> term
         | [def_var]


def_var ::= string

type_def ::= type def_var = linear_type

term_def ::= let def_var = term

file ::= (type_def | term_def)+
