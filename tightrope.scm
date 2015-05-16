(load "core.scm")
(load "java.scm")
(load "elixir.scm")

(define (usage prog)
  (let ((prompt (string-append "Usage: " prog " [option] <source files>\nOptions:\n"))
        (options '(" -entity        Generate entity code\n"
                   " -serial        Generate serialize/deserialize code\n"
                   " -elixir        Generate codes for Elixir\n"
                   " -java          Generate codes for Java\n"
                   " -d <directory> Specify where to place generated files\n")))
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

(define (do-work entity? serial? elixir? java? dir files)
  (for-each
   (lambda (f)
     (let ((env (read-file f)))
       (if java?
           (begin
             (generate-java-zero-pack env dir)
             (if entity?
                 (generate-java-entities env dir))
             (if serial?
                 (generate-java-serials env dir))))
       (if elixir?
           (begin
             (if entity?
                 (generate-elixir-entities env dir))
             (if serial?
                 (generate-elixir-serials env dir)))))) files))

(define (main argv)
  (let ((prog (car argv)))
    (if (< (length argv) 2)
        (usage prog)
        (let loop ((args (cdr argv))
                   (entity #f)
                   (serial #f)
                   (elixir #f)
                   (java #f)
                   (dir "")
                   (files '()))
          (if (null? args)
              (if (null? files)
                  (usage prog)
                  (let ((last-char (string-ref dir (- (string-length dir) 1))))
                    (if (file-exists? dir)
                        (do-work entity serial elixir java (if (not (char=? last-char #\/)) (string-append dir "/") dir) files)
                        (begin
                          (mkdir-p dir)
                          (do-work entity serial elixir java (if (not (char=? last-char #\/)) (string-append dir "/") dir) files)))))
              (let ((arg (car args)))
                (cond
                 ((equal? arg "-entity")
                  (loop (cdr args) #t serial elixir java dir files))
                 ((equal? arg "-serial")
                  (loop (cdr args) entity #t elixir java dir files))
                 ((equal? arg "-elixir")
                  (loop (cdr args) entity serial #t java dir files))
                 ((equal? arg "-java")
                  (loop (cdr args) entity serial elixir #t dir files))
                 ((equal? arg "-d")
                  (if (> (length (cdr args)) 0)
                      (let ((next (cadr args)))
                        (if (not (char=? (string-ref next 0) #\-))
                            (loop (cddr args) entity serial elixir java next files)
                            (error "main" "output dir not specified")))
                      (error "main" "output dir not specified")))
                 ((not (char=? (string-ref arg 0) #\-))
                  (loop (cdr args) entity serial elixir java dir (cons arg files)))
                 (else
                  (loop (cdr args) entity serial elixir java dir files)))))))))
