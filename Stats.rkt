(define (last ls)
  (define (iter res lst)
    (cond [(null? lst) res]
          [else (iter (car lst) (cdr lst))]
      ))
  (iter (car ls) (cdr ls)))
(define (first-n ls n)
  (cond [(= n 0) '()]
        [else (cons (car ls) (first-n (cdr ls) (- n 1)))]
    ))
(define (bf-n ls n)
  (cond [(= n 0) ls]
        [else (bf-n (cdr ls) (- n 1))]
    ))
(define (merge ls1 ls2)
  (cond [(or (null? ls1) (null? ls2)) (append ls1 ls2)]
        [(< (last ls1) (car ls2)) (append ls1 ls2)]
        [(< (car ls1) (car ls2)) (cons (car ls1) (merge (cdr ls1) ls2))]
        [else (cons (car ls2) (merge ls1 (cdr ls2)))]
    ))
(define (merge-sort ls)
  (cond [(< (length ls) 2) ls]
        [else (merge (merge-sort (first-n ls (floor (/ (length ls) 2))))
                     (merge-sort (bf-n ls (floor (/ (length ls) 2))))
                     )]
    ))
(define (frequencies sorted-ls)
  (define (iter cur a-list)
    (cond [(null? a-list) (list cur)]
          [(= (car a-list) (car cur)) (iter (list (car cur) (+ (cadr cur) 1)) (cdr a-list))]
          [else (cons cur (iter (list (car a-list) 1) (cdr a-list)))]
      ))
  (iter (list (car sorted-ls) 1) (cdr sorted-ls))
  )
(define (mean sorted-ls)
  (define (iter res ls)
    (cond [(null? ls) res]
          [else (iter (+ res (car ls)) (cdr ls))]
      ))
  (/ (iter 0 sorted-ls) (length sorted-ls))
  )
(define (median sorted-ls)
  (cond [(null? sorted-ls) 0]
        [(null? (cdr sorted-ls)) (car sorted-ls)]
        [(null? (cddr sorted-ls)) (mean sorted-ls)]
        [else (median (cdr (first-n sorted-ls (- (length sorted-ls) 1))))]
    ))
(define (keep pred ls)
  (cond [(null? ls) ls]
        [(pred (car ls)) (cons (car ls) (keep pred (cdr ls)))]
        [else (keep pred (cdr ls))]
    ))
(define (modes sorted-ls)
  (define (max-freq freq-ls n)
    (cond [(null? freq-ls) n]
          [(> (cadar freq-ls) n) (max-freq (cdr freq-ls) (cadar freq-ls))]
          [else (max-freq (cdr freq-ls) n)]
      ))
  (let [(maximum (max-freq (cdr (frequencies sorted-ls))
                           (cadar (frequencies sorted-ls))))]
    (map (lambda (x) (car x))
     (keep (lambda (x) (= (cadr x) maximum)) (frequencies sorted-ls)))
      )
  )
(define (accumulate op init seq)
  (cond [(null? seq) init]
        [else (op (car seq)
                  (accumulate op init (cdr seq)))]
    ))
(define (sq x) (* x x))
(define (standard-deviation-p ls)
  (sqrt (/ (accumulate + 0 (map (lambda (x) (sq (- x (mean ls)))) ls))
         (length ls))))
(define (standard-deviation-s ls)
  (sqrt (/ (accumulate + 0 (map (lambda (x) (sq (- x (mean ls)))) ls))
         (- (length ls) 1))))
(define (range a b)
  (cond [(> a b) '()]
        [else (cons a (range (+ a 1) b))]
    ))
(define (pearsons-correlation data1 data2)
  (/ (accumulate + 0 (map (lambda (x) (* (- (list-ref data1 x) (mean data1))
                                         (- (list-ref data2 x) (mean data2))))
                          (range 0 (- (length data1) 1))))
   (sqrt (*
          (accumulate + 0 (map (lambda (x) (sq (- x (mean data1)))) data1))
          (accumulate + 0 (map (lambda (x) (sq (- x (mean data2)))) data2))))))
(define (info)
  (display "This is a program to do statistics on data\n")
  (display "--(merge-sort ls) will sort a list in increasing order\n")
  (display "--(frequencies ls) will return a list containing lists of elements and how many times\n")
  (display "      they occur in the list when given a sorted list\n")
  (display "--(mean ls) will return the mean of a list\n")
  (display "--(median ls) will return the median of a sorted list\n")
  (display "--(modes ls) will return the modes of a sorted list\n")
  (display "--(standard-deviation-p data-set) will return the standard deviation of a set of data\n")
  (display "       assuming the data given is data on the population\n")
  (display "--(standard-deviation-s data-set) will return the standard deviation of a set of data\n")
  (display "       assuming the data given is data a sample\n")
  (display "--(pearsons-correlation data-set1 data-set2) will return the pearsons correlation, r\n")
  (display "       value, of the data sets" ))
