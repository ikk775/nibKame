(define-module nibkame.object
  (use util.match)
  (export make-nibKame-object nibkame-object? attr-ref add-attr make-nibKame-apply make-nibKame-multiple-apply make-nibKame-left-associative-operator)
  )
(select-module nibkame.object)

;;; nibKameのトークンオブジェクトが持つスロット
;;; :type 

(define (alist-add-value-head key alst val)
  (map (lambda (v)
         (if (eq? key (car v))
             `(,(car v) ,val . ,(cdr v))
             v))
       alst))

(define (nibkame-object? obj)
  (and (pair? obj)
       (eq? (car obj)
            'Object)))

(define (make-nibKame-object . options)
  (define make-nibKame-object-sub
    (match-lambda
     [(keyword (vals ...) rests ...)
      `((,keyword ,@vals)
        . ,(make-nibKame-object-sub rests))]
     [() ()]))
  (cons 'Object
        (make-nibKame-object-sub options)))

(define (attr-ref obj attr)
  (assq attr (cdr obj)))

(define (add-attr obj attr val)
  (if (assq attr obj)
      (cons 'Object
            (alist-add-value-head attr (cdr obj) val))
      `(Object
        (,attr ,val)
        ,@(cdr obj))))

(define (make-nibKame-apply f arg)
  (make-nibKame-multiple-apply f `(,arg)))
        
(define (make-nibKame-multiple-apply f args)
  (if (null? args)
      f
      `(Apply ,f ,@args)))

(define make-nibKame-left-associative-operator
  (match-lambda
   [() '()]
   [(e)
    e]
   [(e1 (op e2) rest ...)
    (make-nibKame-multiple-apply op (list e1 (make-nibKame-left-associative-operator (cons e2 rest))))
    ]
   ))
        
(define (make-nibKame-multiple-apply f args)
  (if (null? args)
      f
      `(Apply ,f ,@args)))

(provide "nibkame/object")