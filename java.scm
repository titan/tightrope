(define (>java-name name)
  (let loop ((src (string->list name))
             (upcase? #f)
             (dst '()))
    (if (null? src)
        (list->string (reverse dst))
        (let ((chr (car src))
              (rest (cdr src)))
          (case chr
            ((#\- #\_) (loop rest #t dst))
            (else (if upcase?
                      (loop rest #f (cons (char-upcase chr) dst))
                      (loop rest #f (cons chr dst)))))))))

(define (>java-class-name name)
  (let ((l (string->list (>java-name name))))
    (list->string (cons (char-upcase (car l)) (cdr l)))))

(define (package->dir-name pkg)
  (list->string (map (lambda (x) (if (char=? x #\.) #\/ x)) (string->list pkg))))

(define (has-string-type-deeply? env struct)
  (define (batch-check env types)
    (let loop ((rest types)
               (result #f))
      (if (null? rest)
          result
          (let ((s (get-struct env (symbol->string (car rest)))))
            (if s
                (if (has-string-type-deeply? env s)
                    (loop '() #t)
                    (loop (cdr rest) #f))
                (loop (cdr rest) #f))))))
  (let ((fields (struct-fields struct)))
    (cond
     ((or (> (string-field-count fields) 0) (> (string-array-count fields) 0)) #t)
     ((or (> (custom-field-count fields) 0) (> (custom-array-count fields) 0))
      (let ((custom-fields (filter (lambda (x) (custom-type? (field-type x))) fields))
            (custom-arrays (filter (lambda (x) (custom-array-type? (field-type x))) fields)))
        (let ((result-for-custom-fields (batch-check env (map (lambda (x) (field-type x)) custom-fields)))
              (result-for-custom-arrays (batch-check env (map (lambda (x) (array-base-type (field-type x))) custom-arrays))))
          (or result-for-custom-fields result-for-custom-arrays))))
     (else #f))))

;; generate entity

(define (generate-java-field-declare field)
  (let ((name (>java-name (field-name field)))
        (tag (number->string (field-tag field)))
        (type (field-type field)))
    (if (array-type? type)
        (let ((base-type (array-base-type type)))
          (cond
           ((custom-type? base-type) (string-append "public " (>java-class-name (symbol->string base-type)) " [] " name ";"))
           ((eq? base-type 'string) (string-append "public String [] " name ";"))
           (else (string-append "public " (symbol->string base-type) " [] " name ";"))))
        (cond
         ((custom-type? type) (string-append "public " (>java-class-name (symbol->string type)) " " name ";"))
         ((eq? type 'string) (string-append "public String " name ";"))
         (else (string-append "public " (symbol->string type) " " name ";"))))))

(define (generate-java-entity package struct dir)
  (let* ((name (struct-name struct))
         (fields (struct-fields struct)))
    (let ((pkg (if package (string-append "package " package ";") ""))
          (class-start (string-append "public class " (>java-class-name name) " {"))
          (class-stop "}")
          (field-decls (strcat (map generate-java-field-declare fields))))
      (with-output-to-file
          (string-append dir (>java-class-name name) ".java")
        (lambda ()
          (write-string (string-append pkg class-start field-decls class-stop)))
        (list 'replace)))))

(define (generate-java-entities env dir)
  (let* ((package (get-package env))
         (path (cond
                ((and (> (string-length dir) 0) package) (string-append dir (package->dir-name package) "/"))
                ((> (string-length dir) 0) dir)
                (package (string-append (package->dir-name package) "/"))
                (else ""))))
    (if (and (> (string-length path) 0) (not (file-exists? path)))
        (mkdir-p path))
    (for-each
     (lambda (entity) (generate-java-entity package entity path))
     (get-structs env))))

;; generate serializer

;; generate encoder
(define (generate-java-encoder-init struct-name field idx stridx strcnt objidx objcnt strarridx strarrcnt objarridx objarrcnt)
  (let ((name (>java-name (field-name field)))
        (tag (number->string (field-tag field)))
        (type (field-type field)))
    (let ((attr-name (string-append struct-name "." name)))
      (if (array-type? type)
          (let ((base-type (array-base-type type)))
            (cond
             ((eq? base-type 'byte) (string-append "if (" attr-name " != null) {tags[tlen] = " tag "; tlen ++; dtags[dlen] = " tag "; dlen ++; len += 2 + 4 + " attr-name ".length; count ++;}"))
             ((eq? base-type 'short) (string-append "if (" attr-name " != null) {tags[tlen] = " tag "; tlen ++; dtags[dlen] = " tag "; dlen ++; len += 2 + 4 + " attr-name ".length * 2; count ++;}"))
             ((eq? base-type 'int) (string-append "if (" attr-name " != null) {tags[tlen] = " tag "; tlen ++; dtags[dlen] = " tag "; dlen ++; len += 2 + 4 + " attr-name ".length * 4; count ++;}"))
             ((eq? base-type 'long) (string-append "if (" attr-name " != null) {tags[tlen] = " tag "; tlen ++; dtags[dlen] = " tag "; dlen ++; len += 2 + 4 + " attr-name ".length * 8; count ++;}"))
             ((eq? base-type 'string) (string-append "if (" attr-name " != null) {tags[tlen] = " tag "; tlen ++; dtags[dlen] = " tag "; dlen ++; len += 2 + 4 + 4; " (if (= strarrcnt 1) (string-append " strarrbyte = new byte[" attr-name ".length][]; for (int j = 0; j < " attr-name ".length; j ++) { strarrbyte[j] = " attr-name "[j].getBytes(\"utf-8\"); len += 4 + strarrbyte[j].length;} count ++;} ") (string-append " strarrbytes[" strarridx "] = new byte[" attr-name ".length][]; for (int j = 0; j < " attr-name ".length; j ++) { strarrbytes[" strarridx "][j] = " attr-name "[j].getBytes(\"utf-8\"); len += 4 + strarrbytes[" strarridx "][j].length;} count ++;} "))))
             (else (string-append "if (" attr-name " != null) {tags[tlen] = " tag "; tlen ++; dtags[dlen] = " tag "; dlen ++; len += 2 + 4 + 4; " (if (= objarrcnt 1) (string-append " objarrbyte = new byte[" attr-name ".length][]; for (int j = 0; j < " attr-name ".length; j ++) { objarrbyte[j] = " (>java-class-name (symbol->string base-type)) "Serializer.encode(" attr-name "[j]); len += 4 + objarrbyte[j].length;} count ++;} ") (string-append " objarrbytes[" objarridx "] = new byte[" attr-name ".length][]; for (int j = 0; j < " attr-name ".length; j ++) { objarrbytes[" objarridx "][j] = " (>java-class-name (symbol->string base-type)) "Serializer.encode(" attr-name "[j]); len += 4 + objarrbytes[" objarridx "][j].length;} count ++;} "))))))
          (cond
           ((eq? type 'byte) (string-append "if (" attr-name " != 0) {tags[tlen] = " tag "; tlen ++; len += 2; count ++;}"))
           ((eq? type 'short) (string-append "if (" attr-name " != 0) {tags[tlen] = " tag "; tlen ++; if (-16384 < " attr-name " && " attr-name " < 16383) { len += 2; } else { len += 2 + 4 + 2; dtags[dlen] = " tag "; dlen ++;} count ++;}"))
           ((eq? type 'int) (string-append "if (" attr-name " != 0) {tags[tlen] = " tag "; tlen ++; if (-16384 < " attr-name " && " attr-name " < 16383) { len += 2; } else { len += 2 + 4 + 4; dtags[dlen] = " tag "; dlen ++;} count ++;}"))
           ((eq? type 'long) (string-append "if (" attr-name " != 0) {tags[tlen] = " tag "; tlen ++; if (-16384 < " attr-name " && " attr-name " < 16383) { len += 2; } else { len += 2 + 4 + 8; dtags[dlen] = " tag "; dlen ++;} count ++;}"))
           ((eq? type 'string) (string-append "if (" attr-name " != null) {tags[tlen] = " tag "; tlen ++; dtags[dlen] = " tag "; dlen ++;" (if (= strcnt 1) (string-append " strbyte = " attr-name ".getBytes(\"utf-8\"); len += 2 + 4 + strbyte.length;") (string-append " strbytes[" stridx "] = " attr-name ".getBytes(\"utf-8\"); len += 2 + 4 + strbytes[" stridx "].length;")) " count ++;}"))
           (else (string-append "if (" attr-name " != null) {tags[tlen] = " tag "; tlen ++; dtags[dlen] = " tag "; dlen ++;" (if (= objcnt 1) (string-append " objbyte = " (>java-class-name (symbol->string type)) "Serializer.encode(" attr-name "); len += 2 + 4 + objbyte.length;") (string-append "objbytes[" objidx "] = " (>java-class-name (symbol->string type)) "Serializer.encode(" attr-name "); len += 2 + 4 + objbytes[" objidx "].length;")) "count ++;}")))))))

(define (generate-java-encoder-inits struct)
  (let loop ((fields (struct-fields struct))
             (idx 0)
             (stridx 0)
             (objidx 0)
             (strarridx 0)
             (objarridx 0)
             (inits '()))
    (if (null? fields)
        (let ((adjust-count "if (count != 0) {if (tags[0] != 0) { len += 2; count ++;}} if (tlen > 1) {for (short i = 1; i < tlen; i ++) {if (tags[i - 1] + 1 != tags[i]) {len +=2; count ++;}}}")
              (buffer-init "ByteBuffer buf = ByteBuffer.allocate(len); buf.putShort(count);"))
          (string-append (strcat (reverse inits)) adjust-count buffer-init))
        (let* ((field (car fields))
               (type (field-type field)))
          (loop
           (cdr fields)
           (+ idx 1)
           (if (eq? 'string type) (+ stridx 1) stridx)
           (if (custom-type? type) (+ objidx 1) objidx)
           (if (string-array-type? type) (+ strarridx 1) strarridx)
           (if (custom-array-type? type) (+ objarridx 1) objarridx)
           (cons (generate-java-encoder-init
                  (>java-name (struct-name struct))
                  field
                  (number->string idx)
                  (number->string stridx)
                  (string-field-count (struct-fields struct))
                  (number->string objidx)
                  (custom-field-count (struct-fields struct))
                  (number->string strarridx)
                  (string-array-count (struct-fields struct))
                  (number->string objarridx)
                  (custom-array-count (struct-fields struct)))
                 inits))))))

(define (generate-java-encoder-set-field struct-name field idx)
  (let ((name (>java-name (field-name field)))
        (tag (number->string (field-tag field)))
        (type (field-type field)))
    (let* ((attr-name (string-append struct-name "." name))
           (set
            (cond
             ((eq? type 'byte) (string-append "buf.putShort((short)((" attr-name " + 1) * 2));"))
             ((eq? type 'short) (string-append "if (-16384 < " attr-name " && " attr-name " < 16383) {" "buf.putShort((short)((" attr-name " + 1) * 2));} else {buf.putShort((short) 0);}"))
             ((eq? type 'int) (string-append "if (-16384 < " attr-name " && " attr-name " < 16383) {" "buf.putShort((short)((" attr-name " + 1) * 2));} else {buf.putShort((short) 0);}"))
             ((eq? type 'long) (string-append "if (-16384 < " attr-name " && " attr-name " < 16383) {" "buf.putShort((short)((" attr-name " + 1) * 2));} else {buf.putShort((short) 0);}"))
             (else "buf.putShort((short) 0);"))))
      (string-append "case " tag ":" set " break;"))))

(define (generate-java-encoder-set-fields struct)
  (let loop ((fields (struct-fields struct))
             (idx 0)
             (sets '()))
    (if (null? fields)
        (let ((loop-start "for (short i = 0; i < tlen; i ++) {")
              (loop-stop "}")
              (insert-skip-field "if (i == 0) {if (tags[0] != 0) {buf.putShort((short)((tags[0]) * 2 + 1));}} else {if (tags[i - 1] + 1 != tags[i]) {buf.putShort((short)((tags[i] - tags[i - 1] - 1) * 2 + 1));}}")
              (switch-start "switch (tags[i]) {")
              (switch-stop "}"))
          (string-append loop-start insert-skip-field switch-start (strcat (reverse sets)) switch-stop loop-stop))
        (let ((field (car fields)))
          (loop (cdr fields) (+ idx 1) (cons (generate-java-encoder-set-field (>java-name (struct-name struct)) field (number->string idx)) sets))))))

(define (generate-java-encoder-set-data struct-name field idx stridx strcnt objidx objcnt strarridx strarrcnt objarridx objarrcnt)
  (let ((name (>java-name (field-name field)))
        (tag (number->string (field-tag field)))
        (type (field-type field)))
    (let* ((attr-name (string-append struct-name "." name))
           (set
            (if (array-type? type)
                (cond
                 ((eq? (array-base-type type) 'byte) (string-append "buf.putInt(" attr-name ".length); buf.put(" attr-name ");"))
                 ((eq? (array-base-type type) 'short) (string-append "buf.putInt(" attr-name ".length); for (int j = 0; j < " attr-name ".length; j ++) { buf.putShort(" attr-name "[j]);}"))
                 ((eq? (array-base-type type) 'int) (string-append "buf.putInt(" attr-name ".length); for (int j = 0; j < " attr-name ".length; j ++) { buf.putInt(" attr-name "[j]);}"))
                 ((eq? (array-base-type type) 'long) (string-append "buf.putInt(" attr-name ".length); for (int j = 0; j < " attr-name ".length; j ++) { buf.putLong(" attr-name "[j]);}"))
                 ((eq? (array-base-type type) 'string) (let ((bs (if (> strarrcnt 1) (string-append " strarrbytes[" strarridx "][j]") " strarrbyte[j]"))) (string-append "sum = 0; for (int j = 0; j < " attr-name ".length; j ++) {byte [] bs = " bs "; sum += bs.length;} buf.putInt(sum); buf.putInt(" attr-name ".length); for (int j = 0; j < " attr-name ".length; j ++) { byte [] bs = " bs "; buf.putInt(bs.length); buf.put(bs);}")))
                 (else (let ((bs (if (> objarrcnt 1) (string-append " objarrbytes[" objarridx "][j]") "objarrbyte[j]"))) (string-append "sum = 0; for (int j = 0; j < "attr-name".length; j ++) {byte [] bs = "bs"; sum += bs.length;} buf.putInt(sum); buf.putInt(" attr-name ".length); for (int j = 0; j < " attr-name ".length; j ++) { byte [] bs = " bs "; buf.putInt(bs.length); buf.put(bs);}"))))
                (cond
                 ((eq? type 'byte) (string-append "buf.putInt(1);buf.put(" attr-name ");"))
                 ((eq? type 'short) (string-append "buf.putInt(2);buf.putShort(" attr-name ");"))
                 ((eq? type 'int) (string-append "buf.putInt(4);buf.putInt(" attr-name ");"))
                 ((eq? type 'long) (string-append "buf.putLong(8);buf.putLong(" attr-name ");"))
                 ((eq? type 'string) (if (= strcnt 1) "buf.putInt(strbyte.length);buf.put(strbyte);" (string-append "buf.putInt(strbytes[" stridx "].length);buf.put(strbytes[" stridx "]);")))
                 (else (if (= objcnt 1) "buf.putInt(objbyte.length);buf.put(objbyte);" (string-append "buf.putInt(objbytes[" objidx "].length);buf.put(objbytes[" objidx "]);" )))))))
      (string-append "case " tag ":" set " break;"))))

(define (generate-java-encoder-set-datas struct)
  (let loop ((fields (struct-fields struct))
             (idx 0)
             (stridx 0)
             (objidx 0)
             (strarridx 0)
             (objarridx 0)
             (sets '()))
    (if (null? fields)
        (let ((loop-start "for (short i = 0; i < dlen; i ++) { switch (dtags[i]) {")
              (loop-stop "}}"))
          (string-append (if (or (> (string-array-count (struct-fields struct)) 0) (> (custom-array-count (struct-fields struct)) 0)) "int sum = 0;" "") loop-start (strcat (reverse sets)) loop-stop))
        (let* ((field (car fields))
               (type (field-type field)))
          (loop
           (cdr fields)
           (+ idx 1)
           (if (eq? 'string type) (+ stridx 1) stridx)
           (if (custom-type? type) (+ objidx 1) objidx)
           (if (string-array-type? type) (+ strarridx 1) strarridx)
           (if (custom-array-type? type) (+ objarridx 1) objarridx)
           (cons (generate-java-encoder-set-data
                  (>java-name (struct-name struct))
                  field
                  (number->string idx)
                  (number->string stridx)
                  (string-field-count (struct-fields struct))
                  (number->string objidx)
                  (custom-field-count (struct-fields struct))
                  (number->string strarridx)
                  (string-array-count (struct-fields struct))
                  (number->string objarridx)
                  (custom-array-count (struct-fields struct)))
                 sets))))))

(define (generate-java-encoder struct throws)
  (let ((name (>java-name (struct-name struct)))
        (fields (struct-fields struct)))
    (let ((fun-start (string-append "public static byte [] encode(" (>java-class-name name) " " name ")" throws " {short count = 0; int len = 2; short [] tags = new short [" (number->string (length fields)) "]; short tlen = 0; short [] dtags = new short [" (number->string (length fields)) "]; short dlen = 0;" (let ((count (string-field-count fields))) (cond ((> count 1) (string-append " byte [][] strbytes = new byte[" (number->string count) "][];")) ((> count 0) " byte [] strbyte = null;") (else ""))) (let ((count (custom-field-count fields))) (cond ((> count 1) (string-append " byte [][] objbytes = new byte[" (number->string count) "][];")) ((> count 0) " byte [] objbyte = null;") (else ""))) (let ((count (string-array-count fields))) (cond ((> count 1) (string-append " byte [][][] strarrbytes = new byte[" (number->string count) "][][];")) ((> count 0) " byte [][] strarrbyte = null;") (else ""))) (let ((count (custom-array-count fields))) (cond ((> count 1) (string-append " byte [][][] objarrbytes = new byte[" (number->string count) "][][];")) ((> count 0) " byte [][] objarrbyte = null;") (else "")))))
          (init (generate-java-encoder-inits struct))
          (set-fields (generate-java-encoder-set-fields struct))
          (set-data (generate-java-encoder-set-datas struct))
          (fun-end " return buf.array();}"))
      (string-append fun-start init set-fields set-data fun-end))))

;; generate decoder
(define (generate-java-decoder-get-field struct-name field idx)
  (let ((name (>java-name (field-name field)))
        (tag (field-tag field))
        (type (field-type field)))
    (let* ((attr-name (string-append struct-name "." name))
           (get
            (cond
             ((eq? type 'byte) (string-append attr-name " = (byte)(v / 2 - 1);"))
             ((eq? type 'short) (string-append attr-name " = (short)(v / 2 - 1);"))
             ((eq? type 'int) (string-append attr-name " = v / 2 - 1;"))
             ((eq? type 'long) (string-append attr-name " = v / 2 - 1;"))
             (else ""))))
      (case type
        ((byte short int long) (string-append "case " idx ":" get " break;"))
        (else "")))))

(define (generate-java-decoder-get-fields struct)
  (let loop ((fields (struct-fields struct))
             (idx 0)
             (gets '()))
    (if (null? fields)
        (let ((loop-start "for (short i = 0; i < count; i ++) { short v = buf.getShort(); if ((v & (short)0x01) == 1) { tag += (v - 1) / 2; } else if (v == 0) { dtags[dlen] = tag; dlen ++; tag ++; } else {")
              (loop-stop "tag ++;}}")
              (switch-start "switch (tag) {")
              (switch-stop "default: break;}"))
          (string-append loop-start switch-start (strcat (reverse gets)) switch-stop loop-stop))
        (let ((field (car fields)))
          (loop (cdr fields) (+ idx 1) (cons (generate-java-decoder-get-field (>java-name (struct-name struct)) field (number->string idx)) gets))))))

(define (generate-java-decoder-get-data struct-name field idx stridx strcnt objidx objcnt)
  (let ((name (>java-name (field-name field)))
        (tag (number->string (field-tag field)))
        (type (field-type field)))
    (let* ((attr-name (string-append struct-name "." name))
           (get
            (if (array-type? type)
                (cond
                 ((eq? (array-base-type type) 'byte) (string-append "{int len = buf.getInt(); byte [] tmp = new byte [len]; buf.get(tmp); " attr-name " = tmp;}"))
                 ((eq? (array-base-type type) 'short) (string-append "{int len = buf.getInt(); short [] tmp = new short [len]; for (int j = 0; j < len; j ++) { tmp[j] = buf.getShort(); } " attr-name " = tmp;}"))
                 ((eq? (array-base-type type) 'int) (string-append "{int len = buf.getInt(); int [] tmp = new int [len]; for (int j = 0; j < len; j ++) { tmp[j] = buf.getInt(); } " attr-name " = tmp;}"))
                 ((eq? (array-base-type type) 'long) (string-append "{int len = buf.getInt(); long [] tmp = new long [len]; for (int j = 0; j < len; j ++) { tmp[j] = buf.getLong(); } " attr-name " = tmp;}"))
                 ((eq? (array-base-type type) 'string) (string-append "{int total = buf.getInt(); int len = buf.getInt(); String [] tmp = new String [len]; for (int j = 0; j < len; j ++) { int l = buf.getInt(); byte [] b = new byte[l]; buf.get(b); tmp[j] = new String(b, \"utf-8\"); } " attr-name " = tmp;}"))
                 (else
                  (let ((custom-class-name (>java-class-name (symbol->string (array-base-type type)))))
                    (string-append "{int total = buf.getInt(); int len = buf.getInt(); " custom-class-name " [] tmp = new " custom-class-name "[len]; for (int j = 0; j < len; j ++) { int l = buf.getInt(); byte [] b = new byte[l]; buf.get(b); tmp[j] = " custom-class-name "Serializer.decode(b); } " attr-name " = tmp; }"))))
                (cond
                 ((eq? type 'byte) (string-append "buf.getInt();" attr-name " = buf.get();"))
                 ((eq? type 'short) (string-append "buf.getInt();" attr-name " = buf.getShort();"))
                 ((eq? type 'int) (string-append "buf.getInt();" attr-name " = buf.getInt();"))
                 ((eq? type 'long) (string-append "buf.getInt();" attr-name " = buf.getLong();"))
                 ((eq? type 'string) (string-append "{ int len = buf.getInt(); byte tmp [] = new byte[len]; buf.get(tmp); " attr-name " = new String(tmp, \"utf-8\"); }"))
                 (else
                  (let ((custom-class-name (>java-class-name (symbol->string type))))
                    (string-append "{ int len = buf.getInt(); byte tmp [] = new byte[len]; buf.get(tmp); " attr-name " = " custom-class-name "Serializer.decode(tmp);}")))))))
      (string-append "case " tag ":" get " break;"))))

(define (generate-java-decoder-get-datas struct)
  (let loop ((fields (struct-fields struct))
             (idx 0)
             (stridx 0)
             (objidx 0)
             (gets '()))
    (if (null? fields)
        (let ((loop-start "for (short i = 0; i < dlen; i ++) { switch (dtags[i]) {")
              (loop-stop "}}"))
          (string-append loop-start (strcat (reverse gets)) loop-stop))
        (let ((field (car fields)))
          (loop (cdr fields) (+ idx 1) (if (eq? 'string (field-type field)) (+ stridx 1) stridx) (if (custom-type? (field-type field)) (+ objidx 1) objidx) (cons (generate-java-decoder-get-data (>java-name (struct-name struct)) field (number->string idx) (number->string stridx) (string-field-count fields) (number->string objidx) (custom-field-count fields)) gets))))))

(define (generate-java-decoder struct throws)
  (let ((name (>java-name (struct-name struct)))
        (fields (struct-fields struct)))
    (let ((fun-start (string-append "public static " (>java-class-name name) " decode(byte [] bytes)" throws " { ByteBuffer buf = ByteBuffer.wrap(bytes); short count = buf.getShort(); if (count > 0) { " (>java-class-name name) " " name " = new " (>java-class-name name) "(); short [] dtags = new short[" (number->string (length fields)) "]; int dlen = 0; short tag = 0;"))
          (fun-end (string-append "return " name ";} return null;}"))
          (get-fields (generate-java-decoder-get-fields struct))
          (get-data (generate-java-decoder-get-datas struct)))
      (string-append fun-start get-fields get-data fun-end))))

(define (generate-java-serial env package struct dir)
  (let ((name (>java-name (struct-name struct)))
        (fields (struct-fields struct)))
    (let ((throws (if (has-string-type-deeply? env struct) " throws java.io.UnsupportedEncodingException" "")))
      (let ((pkg (if package (string-append "package " package ";") ""))
            (import "import java.nio.ByteBuffer;")
            (class-start (string-append "public class " (>java-class-name name) "Serializer {"))
            (class-stop "}")
            (encode-fun (generate-java-encoder struct throws))
            (encode-zero-pack-fun (string-append "public static byte [] encode0Pack(" (>java-class-name name) " " name ")" throws " { return ZeroPack.pack(encode(" name "));}"))
            (decode-fun (generate-java-decoder struct throws))
            (decode-zero-pack-fun (string-append "public static " (>java-class-name name) " decode0Pack(byte [] bytes)" throws " { return decode(ZeroPack.unpack(bytes));}")))
        (with-output-to-file
            (string-append dir (>java-class-name name) "Serializer.java")
          (lambda ()
            (write-string (string-append pkg import class-start encode-fun "\n" encode-zero-pack-fun "\n" decode-fun "\n" decode-zero-pack-fun class-stop)))
          (list 'replace))))))

(define (generate-java-serials env dir)
  (let* ((package (get-package env))
         (path (cond
                ((and (> (string-length dir) 0) package) (string-append dir (package->dir-name package) "/"))
                ((> (string-length dir) 0) dir)
                (package (string-append (package->dir-name package) "/"))
                (else ""))))
    (if (and (> (string-length path) 0) (not (file-exists? path)))
        (mkdir-p path))
    (for-each
     (lambda (entity) (generate-java-serial env package entity path))
     (get-structs env))))

(define (generate-java-zero-pack env dir)
  (let* ((package (get-package env))
         (path (cond
                ((and (> (string-length dir) 0) package) (string-append dir (package->dir-name package) "/"))
                ((> (string-length dir) 0) dir)
                (package (string-append (package->dir-name package) "/"))
                (else ""))))
    (let ((pkg (if package (string-append "package " package ";") ""))
          (src "import java.nio.ByteBuffer;

public class ZeroPack {
    public static byte [] pack(byte [] inp) {
        ByteBuffer buf = ByteBuffer.allocate(inp.length);
        byte [] bytes = new byte[8];
        int ffpos = 0;
        int ffcnt = 0;
        int oopos = 0;
        int oocnt = 0;
        for (int i = 0, len = inp.length % 8 == 0? inp.length / 8: inp.length / 8 + 1; i < len; i ++) {
            byte bitmap = 0;
            int k = 0;
            for (int j = 0, jlen = i == inp.length / 8 ? inp.length % 8: 8; j < jlen; j ++) {
                byte b = inp[i * 8 + j];
                if (b != (byte)0) {
                    bitmap = (byte)(bitmap | (1 << (8 - j - 1)));
                    bytes[k] = b;
                    k ++;
                }
            }
            if (bitmap == (byte)0xFF) {
                if (oocnt > 0) {
                    int tmp = buf.position();
                    buf.position(oopos);
                    buf.put((byte)oocnt);
                    buf.position(tmp);
                    oocnt = 0;
                }
                if (ffcnt == 0) {
                    buf.put((byte) 0xFF);
                    ffpos = buf.position();
                    buf.position(ffpos + 1);
                    ffcnt ++;
                } else if (ffcnt == 0xFF) {
                    int tmp = buf.position();
                    buf.position(ffpos);
                    buf.put((byte) 0xFF);
                    buf.position(tmp);
                    ffcnt = 0;
                } else {
                    ffcnt ++;
                }
                for (int l = 0; l < k; l ++) {
                    buf.put(bytes[l]);
                }
            } else if (bitmap == (byte)0x00) {
                if (ffcnt > 0) {
                    int tmp = buf.position();
                    buf.position(ffpos);
                    buf.put((byte)ffcnt);
                    buf.position(tmp);
                    ffcnt = 0;
                }
                if (oocnt == 0) {
                    buf.put((byte) 0x00);
                    oopos = buf.position();
                    buf.position(oopos + 1);
                    oocnt ++;
                } else if (oocnt == 0xFF) {
                    int tmp = buf.position();
                    buf.position(oopos);
                    buf.put((byte) 0xFF);
                    buf.position(tmp);
                    oocnt = 0;
                } else {
                    oocnt ++;
                }
            } else {
                buf.put(bitmap);
                if (ffcnt > 0) {
                    int tmp = buf.position();
                    buf.position(ffpos);
                    buf.put((byte)ffcnt);
                    buf.position(tmp);
                    ffcnt = 0;
                } else if (oocnt > 0) {
                    int tmp = buf.position();
                    buf.position(oopos);
                    buf.put((byte)oocnt);
                    buf.position(tmp);
                    oocnt = 0;
                }
                for (int l = 0; l < k; l ++) {
                    buf.put(bytes[l]);
                }
            }
        }
        if (ffcnt > 0) {
            int tmp = buf.position();
            buf.position(ffpos);
            buf.put((byte)ffcnt);
            buf.position(tmp);
            ffcnt = 0;
        } else if (oocnt > 0) {
            int tmp = buf.position();
            buf.position(oopos);
            buf.put((byte)oocnt);
            buf.position(tmp);
            oocnt = 0;
        }
        byte [] out = new byte[buf.capacity() - buf.remaining()];
        buf.rewind();
        buf.get(out);
        return out;
    }

    public static byte [] zeros = {0, 0, 0, 0, 0, 0, 0, 0};

    public static byte [] unpack(byte [] inp) {
        return unpack(inp, 0);
    }

    public static byte [] unpack(byte [] inp, int recommand) {
        ByteBuffer buf = ByteBuffer.allocate(recommand == 0? inp.length * 2 + inp.length / 2: recommand);
        int ptr = 0;
        int cnt = 0;
        while (ptr < inp.length) {
            byte b = inp[ptr];
            switch (b) {
            case 0:
                cnt = inp[ptr + 1];
                for (int i = 0; i < cnt; i ++) {
                    buf.put(zeros);
                }
                ptr += 2;
                break;
            case (byte)0xFF:
                cnt = inp[ptr + 1];
                buf.put(inp, ptr + 2, cnt * 8);
                ptr += 2 + cnt * 8;
                break;
            default:
                cnt = 0;
                for (int i = 0; i < 8; i ++) {
                    if ((b & (1 << (8 - i - 1))) > 0) {
                        cnt ++;
                        byte data = inp[ptr + cnt];
                        buf.put(data);
                    } else {
                        buf.put((byte)0);
                    }
                }
                ptr += cnt + 1;
                break;
            }
        }
        byte [] out = new byte[buf.capacity() - buf.remaining()];
        buf.rewind();
        buf.get(out);
        return out;
    }
}
"))
      (if (and (> (string-length path) 0) (not (file-exists? path)))
          (mkdir-p path))
      (with-output-to-file
          (string-append path "ZeroPack.java")
        (lambda ()
          (write-string (string-append pkg src)))
        (list 'replace)))))
