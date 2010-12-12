(if (not (member "." *load-path*))
    (add-load-path "." *load-path*))

(use parser.peg)
(use srfi-14)
(use util.match)
(use nibkame.object)

(define bs-escape-char-alist
  '((#\\ . #\\)
    (#\" . #\")
    (#\' . #\')
    (#\f . #\x0c)
    (#\t . #\tab)
    (#\r . #\return)
    (#\n . #\newline)
    (#\0 . #\null)
    ))

(define ($keyword id)
  ($trailing-spaces
   ($do [str ($string id)]
        ($not ($or alphanum ($one-of #[_']))))))

(define ($be-signed parser . args)
  (let-optionals* args ([r -] [id +])
    ($do [s ($optional ($one-of #[+-]))]
         [n parser]
         ($return (if (equal? s #\-)
                      (r n)
                      n)
                      ))))

(define ($return-tag&value tag val parser)
  ($do parser
       ($return (make-nibKame-object :type `(,tag) :value `(,val)))))

(define ($return-operator operator parser)
  ($return-tag&value 'Operator operator parser))

(define ($return-value . args)
  ((match-lambda
    [((tags ...) parser)
     ($do [val parser]
          ($return (make-nibKame-object :type tags :value `(,val))))]
    [(tag parser)
     ($do [val parser]
          ($return (make-nibKame-object :type `(,tag) :value `(,val))))]
    )
   args))

(define ($add-type-tag tag parser)
  ($do [n parser]
    ($return (add-attr n :type tag))))


(define ($trailing main trailing)
  ($do [r main]
       trailing
       ($return r)))

(define ($trailing-spaces parser)
  ($do [r parser]
       ($many spaces)
       ($return r)))

(define ($apply f parser)
  ($do [r parser]
       ($return (f r))))

(define ($infix-operator lvalue operator rvalue)
  ($do [l lvalue]
       [op operator]
       [r rvalue]
       ($return
        (make-nibKame-multiple-apply
         op
         `(,l ,r)))))

(define ($seq-to-list . parsers)
  (if (null? parsers)
      ($return '())
      ($do [a (car parsers)]
           [b (apply $seq-to-list (cdr parsers))]
           ($return
            (cons a b)))))

(define x #f)

(define (parse-program str)
(letrec
 (
  [dot ($char #\.)]
  [bs ($char #\\)]
  [squote ($char #\')]
  [dquote ($char #\")]
  [qquote ($char #\`)]
  [lparen ($char #\()]
  [rparen ($char #\))]
  [langle ($char #\<)]
  [rangle ($char #\>)]
  [plus ($char #\+)]
  [minus ($char #\-)]
  [star ($char #\*)]
  [slash ($char #\/)]
  [equal-sign ($char #\=)]
  [any ($char #\_)]
  [semicolon ($char #\;)]
  [exclamation ($char #\!)]
  
  [keyword-if ($keyword "if")]
  [keyword-then ($keyword "then")]
  [keyword-else ($keyword "else")]
  [keyword-let ($keyword "let")]
  [keyword-match ($keyword "match")]
  [keyword-when ($keyword "when")]
  [keyword-with ($keyword "with")]
  [keyword-fun ($keyword "fun")]
  [keyword-function ($keyword "function")]
  [keyword-in ($keyword "in")]
  [keyword-arrow ($keyword "->")]
  [keyword-pipe ($keyword "|")]
  [keyword-equal ($keyword "=")]
  [keywords ($or keyword-if keyword-then keyword-else keyword-let keyword-match keyword-when keyword-with keyword-fun keyword-function)]

  [add-e   ($return-operator "+" plus)]
  [add-d   ($return-operator "+." ($seq plus dot))]
  [add ($or add-e add-d)]
  [sub-e   ($return-operator "-" minus)]
  [sub-d   ($return-operator "-." ($seq minus dot))]
  [sub ($or sub-e sub-d)]
  [mul-e   ($return-operator "*" star)]
  [mul-d   ($return-operator "*." ($seq star dot))]
  [mul ($or mul-e mul-d)]
  [div-e   ($return-operator "/" slash)]
  [div-d   ($return-operator "/." ($seq slash dot))]
  [div ($or div-e div-d)]

  [arithmetic-operator ($or add sub mul div)]
             
  [equal ($return-operator "=" equal-sign)]
  [not-equal ($return-operator "<>" ($seq langle rangle))]
  [less-equal ($return-operator "<=" ($seq langle equal-sign))]
  [less ($return-operator "<" langle)]
  [greater-equal ($return-operator "<=" ($seq rangle equal-sign))]
  [greater ($return-operator ">" rangle)]
  [struct-equal ($return-operator "==" ($seq equal-sign equal-sign))]
  [struct-not-equal ($return-operator "!=" ($seq exclamation equal))]
  [compare ($add-type-tag 'compare ($or equal not-equal less-equal less greater-equal greater struct-equal struct-not-equal))]

  [assign ($return-operator 'assign ($seq langle minus))]

  [operator ($trailing-spaces ($or arithmetic-operator compare assign))]

  [bs-escaped-char ($do bs
                        [c ($one-of (list->char-set (map car bs-escape-char-alist)))]
                        ($return (cdr (assq c bs-escape-char-alist))))]
  [hex-escaped-char ($do bs
                         (char #\u)
                         [x ($many hexdigit)]
                         semicolon
                         ($return (ucs->char (string->number x 16))))]
  [string-sub ($between dquote
                        ($many ($or ($none-of (char-set #\" #\\))
                                    bs-escaped-char
                                    hex-escaped-char))
                        dquote)]
  [string ($return-value 'String
                        ($apply list->string string-sub))]
  [char-sub ($between squote
                      ($many ($or ($none-of (char-set #\' #\\))
                                  bs-escaped-char
                                  hex-escaped-char))
                      squote)]
  [char ($return-value 'Char char-sub)]
  
  [integer-dec ($do [n ($many digit 1)]
                    ($return (string->number (list->string n))))]
  [integer-dec-signed ($be-signed integer-dec)]
  [integer ($return-value 'Integer ($or integer-dec-signed))]
  
  [real-exp-dec ($seq ($one-of #[eE])
                      integer-dec-signed)]
  [real-dec-positive ($do (i integer-dec)
                          dot
                          (sf ($many digit))
                          (e ($optional real-exp-dec))
                          (let ([lf (length sf)]
                                [f (string->number (list->string sf))]
                                [e (if e e 0)])
                            ($return (* (+ i (/ f (expt 10 lf)))
                                        (expt 10 e)))
                            ))]
  [real-dec ($be-signed real-dec-positive)]
  [real ($return-value 'Real ($or real-dec))]
  
  [number-exact ($add-type-tag 'Exact ($or integer))]
  [number-inexact ($add-type-tag 'Inexact ($or integer))]
  [number ($add-type-tag 'Number ($or real integer))]
  
  [ident-name ($seq ($not keywords) ($many ($or digit lower upper any) 1))]

  [ident ($return-value 'Ident ($apply list->string ident-name))]

  [functional-operator ($between lparen ($trailing-spaces operator) rparen)]
  
  [object ($or number string functional-operator ident)]
  
  [function-call ($do [f function-call-element]
                      [args ($many function-call-element)]
                      ($return (make-nibKame-multiple-apply f args)))]
  [function-call-element ($trailing-spaces
                           ($or ($between lparen expr rparen)
                                object))]
  [if-form
         ($do
          keyword-if
          [c expr]
          keyword-then
          [e1 expr]
          keyword-else
          [e2 expr]
          ($return `(If ,c ,e1 ,e2)))]
  [let-form
   ($do keyword-let
        [v ident]
        keyword-equal
        [e1 expr]
        keyword-in
        [e2 expr]
        ($return `(Let ,v ,e1 ,e2)))]
  [factor ($trailing-spaces
           ($or function-call
                ($between lparen expr rparen)
                object
                ))]
  [term ($do [a factor]
             [b ($many ($seq-to-list operator factor))]
             ($return (make-nibKame-left-associative-operator (cons a b))))]
  [expr ($trailing-spaces ($or if-form term))]
  [program ($trailing expr eof)]
 )
 (parse-string program str)
 )
)

(define translate
  (match-lambda
   [('Object (:type 'Ident) (:value x))
    (string->symbol x)]
   [('Object (:type 'String) (:value x))
    x]
   [('Object (:type 'Number 'Integer) (:value x))
    x]
   [('Object (:type 'Number 'Real) (:value x))
    (exact->inexact x)]
   [('Object (:type 'Operator) (:value x))
    (string->symbol x)]
   [('Object (:type 'compare 'Operator) (:value x))
    (string->symbol x)]
   [('If c e1 e2)
    `(if ,@(map translate (list c e1 e2)))]
   [('Apply f args ...)
    `(apply ,@(map translate (cons f args)))]
   [e e]
   ))

#?=(translate (parse-program  "func \"abc\" 1 2 (another_func argument)"))
#?=(translate (parse-program  "func arg1 arg2 arg3"))
#?=(translate (parse-program  "symbol + x"))
#?=(translate (parse-program  "\"string\\n\""))
#?=(translate (parse-program  "real -3.4332e32"))
#?=(translate (parse-program  "if -3.4332e32 > 0 then 1 else 2"))