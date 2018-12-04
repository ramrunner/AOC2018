(use extras vector-lib srfi-13 srfi-1)

;;stores events, loads and sort them.
(define (events)
  (let* ((ostore '())
         (ordstore '()))
    (define (rd fname)
      (set! ostore (with-input-from-file fname read-lines)))
    (define (totsecs lst) (+ (cadddr lst) (* 60 (caddr lst)) (* 1440 (cadr lst)) (* 44640 (car lst))))
    (define (sortstore) (let ((store (map (lambda (s) 
                                          (cons (totsecs (map string->number (string-split (substring s 6 17) "-: ")))
                                          s)) ostore)))
                   (set! ordstore (sort store (lambda (i j) (< (car i) (car j)))))))
    (define (print)
      (format #t "store:~A~%" ordstore))
    (define (dispatch m)
      (cond ((eq? m 'read) rd)
            ((eq? m 'sort) sortstore)
            ((eq? m 'getstore) ordstore)
            ((eq? m 'print) print)))
    dispatch))

;;stores a guard association. either with name, sl/wakeup sequence lists, or with 60-minute vectors.
(define (guards)
  (let* ((gstore '())
         (slstore '())
         (minstore '()))
    (define (act num time)
      (set! gstore (alist-update num (append (alist-ref num gstore eqv? '()) (list time)) gstore)))
    (define (getstore) gstore)
    (define (getsleeps) (sort (map (lambda (e) (cons (caar e) (fold-right + 0 (cdar e)))) slstore)
                              (lambda (a b) (< (cdr a) (cdr b)))))
    (define (csleeps) (set! slstore (map (lambda (e) (alist-cons (car e) (calcdiffs (cdr e)) slstore)) gstore)))
    (define (calcsleeps) csleeps)
    (define (cminutes) (set! minstore (map (lambda (e) (alist-cons (car e) (vecmins (cdr e)) minstore)) gstore)))
    (define (dispatch m)
      (cond ((eq? m 'getstore) getstore)
            ((eq? m 'getsleeps) slstore)
            ((eq? m 'getminutes) minstore)
            ((eq? m 'calcsleeps) csleeps)
            ((eq? m 'calcminutes) cminutes)
            ((eq? m 'act) act)))
  dispatch))
     
(define (vector-inc! v i hm)
  (vector-set! v  i (+ hm (vector-ref v i))))

(define (incvec! v m1 m2) 
  (letrec ((loop (lambda (m1 m2) 
                   (if (< m1 m2) 
                       (begin 
                         (vector-inc! v m1 1) 
                         (loop (+ m1 1) m2)))))) 
    (loop m1 m2)))

;;generates a vector for a guard based on his events.
(define (vecmins lst)
  (letrec* ((v (vector-unfold (lambda (x) 0) 60))
            (loop (lambda (lst) (if (not (null? lst)) (begin (incvec! v (car lst) (cadr lst)) (loop (drop lst 2)))))))
    (loop lst)
    v))

;;generates an asleep minute list for a guard based on his events
(define (calcdiffs lst)
  (if (null? lst)
      '()
      (cons (- (cadr lst) (car lst))
            (calcdiffs (drop lst 2)))))

(define (with-guards/minutes g)
  (with-guards g get-minute))

(define (with-guards/total g)
  (with-guards g get-totmin))

;;parsers from the event struct
(define (get-totmin e)
  (car e))

(define (get-minute e)
  (string->number (substring (cdr e) 15 17)))

(define (with-guards g exfunc)
  (let ((cg 0))
   (lambda (e f) 
     (if (string-prefix? "Guard" (substring (cdr e) 19)) 
         (begin 
           (set! cg (string->number (string-trim-both (substring (cdr e) 26 30))))
           (format #t "guard is ~A from ~A ~%" cg (substring (cdr e) 26 30)))
         ((g 'act) cg (exfunc e))))))

(define ev (events))
((ev 'read) "d4input")
((ev 'sort))
(define sortedev (ev 'getstore))
;;part1
(define mg0 (guards))
(fold (with-guards/total mg0) '() sortedev))
((mg0 'calcsleeps))
(define tots (mg0 'getsleeps))
;;gets the sleepiest
(map (lambda (e) (cons (caar e) (fold-right (lambda (a acc) (+ a acc)) 0 (cdar e)))) tots)
;;part1 end and p2
(define mg1 (guards))
(fold (with-guards/minutes mg1) '() sortedev)
(define p1e (mg1 'getstore))
;;assoc to the sleepiest guard and his minute
((mg1 'calcminutes))
(define mins (mg1 'getminutes))
;;gives the guard and the max same minute sleep.
(map (lambda (e) (cons (caar e) (vector-fold (lambda (a b c) (max b c)) 0 (cdar e)))) mins)
