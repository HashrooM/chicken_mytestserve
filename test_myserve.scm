;;string->number at base 16
(string->number "F" 16)
;;=> 15
;;this fuction return #f when arguments cannot be recognized

;;ascii decoding
(integer->char (string->number "3F" 16))
;;=> #\?
;;#\space = white space

;;string->list
(string->list "abc")
;;=>(#\a #\b #\c)

;;optional arguments
(define (test a b . c)
	(* (optional c 1) (+ a b)))
(test 1 2)
;;=> 3 = (+ 1 2)
(test 1 2 2)
;;=> 6 = (* 2 (+ 1 2))

;; function "http-char"
(use srfi-13)
(define (http-char c1 c2)
  (let ((code (string->number
	       (list->string (list c1 c2)) 16)))
    (if code
	(integer->char code)
	'#\space)))
;;test
(http-char #\3 #\F)
;;=> #\? OK

;;function "decode-param"
(define (decode-param s)
  (define (f lst)
    (if (not (null? lst))
	(case (car lst)
	  ('#\% (cons (http-char (cadr lst) (caddr lst))
		      (f (cdddr lst))))
	  ('#\+ (cons #\space (f (cdr lst))))
	  (else (cons (car lst) (f (cdr lst)))))
	lst))
  (list->string (f (string->list s))))

;; string position
(substring "abcdefg" 2 4)
;; => "cd"
(string-index "abcdefg" #\a)
;;=> 0
(string->symbol "foo")
;;=> foo

(define (parse-params s)
  (let ((i1 (string-index s #\=))
	(i2 (string-index s #\&)))
    (if (equal? i2 #f)
	(set! i2 (string-length s)))
    (if i1 (cons (cons (string->symbol (string-upcase (substring s 0 i1)))
		       (decode-param (substring s (+ 1 i1) i2)))
		 (if (not (equal? i2 (string-length s)))
		     (parse-params (substring s (+ 1 i2)))
		     '()))
	s)))

;;another version ... better??
(define (parse-params s)
  (let ((i1 (string-index s #\=))
	(i2 (string-index s #\&)))
    (if i1
	(cons (cons (string->symbol (string-upcase (substring s 0 i1)))
		    (decode-param (substring s (+ i1 1) (if (equal? i2 #f)
							    (string-length s)
							    i2))))
	      (if (equal? i2 #f)
		  '()
		  (parse-params (substring s (+ 1 i2)))))
	s)))

;;; analyzing request header
(define (find-space s)
  (define (search-space s num)
    (let ((pos (string-index s #\space num)))
      (if pos
	  (cons pos (search-space s (+ 1 pos)))
	  '())))
  (search-space s 0))

(define (parse-url s)
  (let* ((url (substring s
			 (+ 2 (car (find-space s)))
			 (cadr (find-space s))))
	 (x (string-index url #\?)))
    (if x
	(cons (substring url 0 x)
	      (parse-params (substring url (+ 1 x))))
	(cons url '()))))

(define (get-header stream)
  (let* ((s (read-line stream))
	 (h (let ((i (if (eof-object? s)
			 #f
			 (string-index s #\:))))
	      (if i
		  (cons (string->symbol (string-upcase (substring s 0 i)))
			(substring s (+ i 2)))
		  #f))))
    (if h
	(cons h (get-header stream))
	'())))

;;test
(call-with-input-string "foo: 1\nbar: abc, 123\n\n"
			(cut get-header <>))
;;=> ((FOO . "1") (BAR . "abc, 123"))

;; analyzint request body
(define (get-content-params stream header)
  (let* ((test (assoc 'CONTENT-LENGTH header))
	 (length (if test 
		     (cdr test)
		     #f)))
    (if length
	(parse-params (read-line stream))
	'())))

;; we can easily extend below code to named let
(let ((test (let-values (((a b) (values 1 2)))
		     (print a b))))
	 test)
;;=> 12

;; server function
(use srfi-34)
(use socket ports)

(define (serve request-handler)
  (let ((sock (socket af/inet sock/stream)))
    (socket-bind sock (inet-address "127.0.0.1" 8000))
    (socket-listen sock 1)
    (define (loop)
      (let ((stream (socket-accept sock)))
	(let* ((url (parse-url (read-line stream)))
	       (path (car url))
	       (header (get-header stream))
	       (params (append (cdr url)
			       (get-content-params stream header))))
	  (request-handler path header params stream))
	(socket-close stream)
	(socket-close sock))
      (loop))))

;; test...hello request handler
(define (hello-request-handler path header params ostream)
  (if (equal? path "greeting")
      (let ((name (assoc 'name params)))
	(if (not name)
	    (begin (display "<form>What is your name?<input name='name' /></form>" ostream)
		   (display (string-join `("Nice to meet you, " ,(cdr name) "!!\n")) ostream))
	    (display "Sorry... I don't know that page.\n" ostream)))))

;; at first, learging how to use socket
;; (use socket)
;; (let ((sock (socket af/inet sock/stream)))
;;        (socket-bind sock (inet-address "127.0.0.1" 8000))
;;        (socket-listen sock 1)
;;        (let ((s (socket-accept sock)))
;; 	 (let-values (((i o) (socket-i/o-ports s)))
;;    (print (read-line i))
;;         (socket-close s)
;;         (socket-close sock)))) 
;; at browser (chroam), searching "localhost:8000/greeting"
;;=> GET /greeting HTTP/1.1
