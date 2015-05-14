(load "core.scm")
(load "java.scm")
(load "elixir.scm")

(define (usage prog)
  (let ((prompt (string-append "Usage: " prog " [option] src0 src1 ... \nOptions:\n"))
        (options '(" -entity        Generate entity code\n"
                   " -serial        Generate serialize/deserialize code\n"
                   " -elixir        Generate codes for Elixir\n"
                   " -java          Generate codes for Java\n")))
    (let loop ((opts options)
               (opts-str ""))
      (if (null? opts)
          (display (string-append prompt opts-str))
          (loop (cdr opts) (string-append opts-str (car opts)))))))

(define (parse-file in)
  (let loop ((datum (read in))
             (ast '()))
    (if (eof-object? datum)
        (eval (reverse ast) '())
        (loop (read in) (cons datum ast)))))

(define (read-file filename)
  (let* ((in (open-input-file filename))
         (ast (parse-file in)))
    (close-input-port in)
    ast))

(define (do-work entity? serial? elixir? java? files)
  (for-each
   (lambda (f)
     (let ((env (read-file f)))
       (if java?
           (begin
             (generate-java-zero-pack env)
             (if entity?
                 (generate-java-entities env))
             (if serial?
                 (generate-java-serials env))))
       (if elixir?
           (begin
             (if entity?
                 (generate-elixir-entities env))
             (if serial?
                 (generate-elixir-serials env)))))) files))

(define (main argv)
  (let ((prog (car argv)))
    (if (< (length argv) 2)
        (usage prog)
        (let loop ((args (cdr argv))
                   (entity #f)
                   (serial #f)
                   (elixir #f)
                   (java #f)
                   (files '()))
          (if (null? args)
              (if (null? files)
                  (usage prog)
                  (do-work entity serial elixir java files))
              (let ((arg (car args)))
                (cond
                 ((equal? arg "-entity")
                  (loop (cdr args) #t serial elixir java files))
                 ((equal? arg "-serial")
                  (loop (cdr args) entity #t elixir java files))
                 ((equal? arg "-elixir")
                  (loop (cdr args) entity serial #t java files))
                 ((equal? arg "-java")
                  (loop (cdr args) entity serial elixir #t files))
                 ((not (char=? (string-ref arg 0) #\-))
                  (loop (cdr args) entity serial elixir java (cons arg files)))
                 (else
                  (loop (cdr args) entity serial elixir java files)))))))))
