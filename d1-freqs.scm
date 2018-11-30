;read the input as a list.
(define (read-list fname)
  (with-input-from-file fname 
                       (lambda ()
                         (letrec ((loop (lambda (line lst)
                                    (if (eof-object? line)
                                        lst
                                        (loop (read-line) (append lst (list (string->number line))))))))
                           (loop (read-line) '())))))

;foldr over the reversed list to maintain order
(define (doit lst) 
  (define allfreq (make-hash-table))
  (define loop (lambda (init) 
    (if (eq? #f (car init))
    (loop (foldr (lambda (elem sum) 
           (let ((found #f))
             (cond ((eq? #t (car sum)) 'END)
                   ((eq? #t (hash-table-exists? allfreq (+ (cdr sum) elem))) (begin (format #t "FOUND:~A~%"  (+ (cdr sum) elem)) (set! found #t) (cons found (+ (cdr sum) elem))))
                   (else (hash-table-set! allfreq (+ (cdr sum) elem) #t) (cons found (+ (cdr sum) elem))))))
             init (reverse lst))))))
  (loop (cons #f 0)))
