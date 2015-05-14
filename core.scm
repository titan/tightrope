(define (reduce acc-fun init items)
  (let loop ((is items)
             (r init))
    (if (null? is)
        r
        (loop (cdr is) (acc-fun r (car is))))))

(define (filter cond-fun items)
  (let loop ((is items)
             (r '()))
    (if (null? is)
        r
        (loop (cdr is) (if (cond-fun (car is)) (cons (car is) r) r)))))

(define (qsort lst comparator)
  (if (null? lst)
      '()
      (let ((x (car lst)))
        (append (qsort (filter (lambda (y) (< (comparator y x) 0)) lst) comparator) (list x) (qsort (filter (lambda (y) (> (comparator y x) 0)) lst) comparator)))))

;; [str0 str1 ...] -> str
(define (strcat strs)
  (reduce (lambda (a x) (string-append a x)) "" strs))

(define (write-string str)
  (for-each (lambda (x) (write-char x)) (string->list str)))

(define (get-package env)
  (let ((p (assoc "package" env)))
    (if p
        (if (= (string-length (car p)) 0)
            #f
            (cdr p))
        #f)))

(define (get-structs env)
  (filter (lambda (x) (not (equal? "package" (car x)))) env))

(define (struct-name struct)
  (car struct))

(define (struct-fields struct)
  (cdr struct))

(define (string-field-count fields)
  (length (filter (lambda (x) (eq? 'string (field-type x))) fields)))

(define (string-array-count fields)
  (length (filter (lambda (x) (string-array-type? (field-type x))) fields)))

(define (custom-field-count fields)
  (length (filter (lambda (x) (custom-type? (field-type x))) fields)))

(define (custom-array-count fields)
  (length (filter (lambda (x) (custom-array-type? (field-type x))) fields)))

(define (custom-type? type)
  (if (array-type? type)
      #f
      (case type
        ((byte short int long string) #f)
        (else #t))))

(define (array-type? type)
  (let* ((typestr (symbol->string type))
         (len (string-length typestr)))
    (char=? #\* (string-ref typestr (- len 1)))))

(define (custom-array-type? type)
  (if (array-type? type)
      (let ((base-type (array-base-type type)))
        (custom-type? base-type))
      #f))

(define (string-array-type? type)
  (if (array-type? type)
      (let ((base-type (array-base-type type)))
        (eq? base-type 'string))
      #f))

(define (array-base-type type)
  (if (array-type? type)
      (let* ((typestr (symbol->string type))
             (len (string-length typestr)))
        (string->symbol (substring typestr 0 (- len 1))))
      type))

(define (field-name field)
  (vector-ref field 0))

(define (field-tag field)
  (vector-ref field 1))

(define (field-type field)
  (vector-ref field 2))

(define (field-len field) ;; only useful for array type
  (if (array-type? (field-type field))
      (if (= (vector-length field) 4)
          (vector-ref field 3)
          0)))

(define (eval-primitive type tag field)
  (let ((name (symbol->string (car field)))
        (rest (cdr field)))
    (list->vector (cons name (cons tag (cons type rest))))))

(define (eval-array type tag field)
  (let ((name (symbol->string (car field)))
        (rest (cdr field)))
    (list->vector (cons name (cons tag (cons type rest))))))

(define (eval-custom type tag field)
  (let ((name (symbol->string (car field)))
        (rest (cdr field)))
    (list->vector (cons name (cons tag (cons type rest))))))

(define (eval-struct name exps)
  (let loop ((es exps)
             (fields '()))
    (if (null? es)
        (cons name (qsort fields (lambda (x y) (cond ((< (field-tag x) (field-tag y)) -1) ((> (field-tag x) (field-tag y)) 1) (else 0)))))
        (let* ((field (car es))
               (type (car field))
               (tag (cadr field))
               (rest (cddr field)))
          (cond
           ((eq? type 'byte) (loop (cdr es) (cons (eval-primitive type tag rest) fields)))
           ((eq? type 'short) (loop (cdr es) (cons (eval-primitive type tag rest) fields)))
           ((eq? type 'int) (loop (cdr es) (cons (eval-primitive type tag rest) fields)))
           ((eq? type 'long) (loop (cdr es) (cons (eval-primitive type tag rest) fields)))
           ((eq? type 'string) (loop (cdr es) (cons (eval-primitive type tag rest) fields)))
           ((eq? type 'byte*) (loop (cdr es) (cons (eval-array type tag rest) fields)))
           ((eq? type 'short*) (loop (cdr es) (cons (eval-array type tag rest) fields)))
           ((eq? type 'int*) (loop (cdr es) (cons (eval-array type tag rest) fields)))
           ((eq? type 'long*) (loop (cdr es) (cons (eval-array type tag rest) fields)))
           ((eq? type 'string*) (loop (cdr es) (cons (eval-array type tag rest) fields)))
           (else (loop (cdr es) (cons (eval-custom type tag rest) fields))))))))

(define (eval-exp e)
  (cond
   ((eq? (car e) 'package) (cons "package" (symbol->string (cadr e))))
   ((eq? (car e) 'struct) (eval-struct (symbol->string (cadr e)) (cddr e)))
   (else (error "eval-exp" "unknown express" e))))

(define (eval ast env)
  (let loop ((as ast)
             (env env))
    (if (null? as)
        env
        (loop (cdr as) (cons (eval-exp (car as)) env)))))
