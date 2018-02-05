;; function "http-char"
(define (http-char c1 c2)
  (let ((code (string->number
	       (list->string (list c1 c2)) 16)))
    (if code
	(integer->char code)
	'#\space)))

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

;;function parse-params
(use srfi-13)
(define (parse-params s)
  (let ((i1 (string-index s #\=))
	(i2 (string-index s #\&)))
    (if i1
	(cons
	 (cons
	  (string->symbol (substring s 0 i1))
	  (decode-param (substring s (+ i1 1) (if (equal? i2 #f)
						  (string-length s)
						  i2))))
	 (if (equal? i2 #f)
	     '()
	     (parse-params (substring s (+ 1 i2)))))
	s)))

;; analyzing request header
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

;; analyzint request body
(define (get-content-params stream header)
  (let* ((test (assoc 'CONTENT-LENGTH header))
	 (length (if test 
		     (cdr test)
		     #f)))
    (if length
	(parse-params (read-line stream))
	'())))

;; server function
(use srfi-34)
(use socket)
(define (serve request-handler)
  (let ((sock (socket af/inet sock/stream)))
    (guard (e (else (socket-close sock) (raise e)))
      (socket-bind sock (inet-address "127.0.0.1" 8080))
      (socket-listen sock 1)
      (define (loop)
	(let ((s (socket-accept sock)))
	  (guard (e (else (begin (socket-close s)
				 (socket-close sock))
			  (raise e)))
	    (let-values (((i o) (socket-i/o-ports s)))
	      (let* ((url (parse-url (read-line i)))
		     (path (car url))
		     (header (get-header i))
		     (params (append (cdr url)
				     (get-content-params i header))))
		(request-handler path header params o)
		(socket-close s)
		(loop))))))
      (loop))))
  
;; request handler...test
(define (hello-request-handler path header params ostream)
  (if (equal? path "greeting")
      (let ((name (assoc 'name params)))
	(if (not name)
	    (display "HTTP/1.1 200 OK\n\n<html><form>What is your name?<input name='name' /></form></html>" ostream)
	    (display (string-join `("HTTP/1.1 200 OK\n\n<html>Nice to meet you, " ,(cdr name) "!!</html>") "") ostream)))
      (display "Sorry... I don't know that page.\n" ostream)))

;; main
;;(serve hello-request-handler)
