(** Utilities for Name Mangling *)
val write_number : int -> string
val read_number : char Stream.t -> int
val write_id : string -> string
val read_id : char Stream.t -> string
val write_seq : ('a -> string) -> 'a list -> string
val read_seq : char Stream.t -> (char Stream.t -> 'a) -> 'a list

(** escape ident *)
val escape : string -> string
val escapex : string -> string -> string

(**
  BNF's of a mangled name are as follows.
{[
  <underline> ::= '_'
  <uppercase> ::= 'A' | ... | 'Z'
  <lowercase> ::= 'a' | ... | 'z'
  <alphabet>  ::= <uppercase> | <lowercase>
  <digit-nonzero> ::= '1' | ... | '9'
  <digit-hexadecimal-nonzero> ::= <digit-nonzero> | 'a' | ... | 'f'
  
  <digit> ::= '0' | <digit-nonzero>
  <digit-hexadecimal> ::= '0' | <digit-hexadecimal-nonzero>
  
  <decimal> ::= <digit-nonzero> { <digit> }
  <hexadecimal> ::= <digit-hexadecimal-nonzero> { <digit-hexadecimal> }
  <hexadecimal-leading-zero> ::= <digit-hexadecimal> { <digit-hexadecimal> }
  
  <symbol> ::= <underline> 'u' 'l'     # '_'
             | <underline> 'p' 'c'     # '%'
             | <underline> 'd' 'r'     # '$'
             | <underline> 'd' 't'     # '.'
             | <underline> 'c' 'm'     # ','
             | <underline> 'c' 'l'     # ':'
             | <underline> 's' 'c'     # ';'
             | <underline> 's' 'p'     # ' '
             | <underline> 'e' 'x'     # '!'
             | <underline> 'q' 'u'     # '?'
             | <underline> 'h' 's'     # '#'
             | <underline> 'q' 't'     # '''
             | <underline> 'q' 'q'     # '`'
             | <underline> 'q' 'w'     # '"'
             | <underline> 'o' 'r'     # '|'
             | <underline> 'e' 't'     # '&'
             | <underline> 'a' 't'     # '@'
             | <underline> 'a' 'c'     # '^'
             | <underline> 'p' 'l'     # '+'
             | <underline> 'm' 'n'     # '-'
             | <underline> 'a' 's'     # '*'
             | <underline> 't' 'l'     # '~'
             | <underline> 's' 'l'     # '/'
             | <underline> 'b' 's'     # '\'
             | <underline> 'e' 'q'     # '='
             | <underline> 'p' 'o'     # '('
             | <underline> 'p' 'c'     # ')'
             | <underline> 'b' 'o'     # '{'
             | <underline> 'b' 'c'     # '}'
             | <underline> 's' 'o'     # '['
             | <underline> 's' 'c'     # ']'
             | <underline> 'l' 't'     # '<'
             | <underline> 'g' 't'     # '>'
             | <underline> 'x' <lowercase> { <alphabet> } <hexadecimal-leading-zero> 'X'  # future extension
             | <underline> 'x' <lowercase> { <alphabet> } <mangled-id>  # future extension
             | <underline> <lowercase> <lowercase>   # future extension
  <letter> ::= <uppercase> | <lowercase> | <symbol>
  <id> ::= { <letter> }

  <mangled-id> ::= <decimal> 's' <id> # where <decimal> is a string length of <id>.
  <tag> ::= <underline> <uppercase>
  
  <tag-prefix-name>       ::= 'N'
  <tag-prefix-unique-id>  ::= 'I'
  <tag-prefix-container>  ::= 'C'
  <tag-prefix-annotation> ::= 'A'
  <tag-prefix>            ::= <tag-prefix-name>
                            | <tag-prefix-unique-id>
                            | <tag-prefix-container>
                            | <uppercase> { <lowercase> } # future extension
  
  <tag-class-source>      ::= 'S'
  <tag-class-sread>       ::= 'S' 'R'
  <tag-class-expr>        ::= 'X'
  <tag-class-type>        ::= 'T'
  <tag-class-typing-type> ::= 'O'
  <tag-class-typing-expr> ::= 'E'
  <tag-class-typing>      ::= 'R'
  <tag-class-typing-pat>  ::= 'R' 'P'
  <tag-class-module>      ::= 'M'
  <tag-class-K-normal>    ::= 'K'
  <tag-class-closure>     ::= 'C'
  
  <tag-class>             ::= <tag-class-source>
                            | <tag-class-expr>
                            | <tag-class-type>
                            | <tag-class-typing-type>
                            | <tag-class-typing-expr>
                            | <tag-class-typing>
                            | <tag-class-module>
                            | <tag-class-K-normal>
                            | <uppercase> { <letter> } # future extension

  <tag-terminater>        ::= <underline> <underline>
  
  <tag>    ::= <underline> <tag-prefix> <tag-class> ( <tag-terminater> )
  
  <mangled-name> ::= { <tag> ( <mangled-id> | <id> ) }
]}
*)
