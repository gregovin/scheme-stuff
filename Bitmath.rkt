(define int-bits 32) ;defines how many bits an int should be
;makes xor a thing, which returns true if one and only one of its inputs is true
(define (xor a b)
  (and (or a b) (not (and a b))))
;Define an or for single bits so that if either bit is 1 1 is returned
(define (bitOr a b)
  (if (or (= a 1) (= b 1))
      1
      0
      ))
;define an and for single bits so that if both bits are 1 a 1 is returned
(define (bitAnd a b)
  (if (and (= a 1) (= b 1))
      1
      0
  ))
;define a xor for single bits
(define (bitXor a b)
  (if (xor (= a 1) (= b 1))
      1
      0
  ))
;define the opposite of a bit
(define (bitNot a)
  (if (= a 1)
      0
      1
      ))
;define a function to make a bitstring the correct size
(define (mkSize ls n)
  ;define a helper function so that we can do this iteratively
  (define (mkSizei ls n a)
    (cond ((= n 0) a)
          ((null? ls) (mkSizei ls (- n 1) (append a '(0))))
          (else (mkSizei (cdr ls) (- n 1) (append a (list (car ls)))))
          ))
  (mkSizei ls n '())
  )
;define a way to add two bits and keep the cary so it is in the form '(sum carry)
(define (half-adder bita bitb)
  (list (bitXor bita bitb) (bitAnd bita bitb)))
;define a way to add to bits and a carry in
(define (full-adder bita bitb cin)
  (list (bitXor cin (bitXor bita bitb)) (bitOr (bitAnd bita bitb) (bitAnd (bitXor bita bitb) cin))))
;define bit-add so that we do a half adder first and then call a helper function
(define (bit-add bitsa bitsb)
  (if (or (null? bitsa) (null? bitsb)) ;if either of them is empty
      (mkSize (append bitsa bitsb) int-bits) ;force them together
      (let ((sum (half-adder (car bitsa) (car bitsb)))) ;let sum be the list of the result and cary out
        (mkSize (append (list (car sum)) (bit-add-helper (cdr bitsa) (cdr bitsb) (cadr sum))) int-bits))
      ;and put the result part of the sum infront of the sum of everything else
     ))
;the helper function for bit-add which repeadedly does the full adder and cary correctly
(define (bit-add-helper bitsa bitsb cin)
  (cond
    ((and (null? bitsa) (null? bitsb)) ;if they are both null
     (if (= cin 1) ;if the last cary was 1
         '(1)
         '()
         ))
    ((null? bitsa) (bit-add-helper '(0) bitsb cin)) ;if one of them is null but not the other, make sure its not null
    ((null? bitsb) (bit-add-helper bitsa '(0) cin))
    (else (let ((sum (full-adder (car bitsa) (car bitsb) cin)));calculate the sum of the first bits and cin
            (append (list (car sum)) (bit-add-helper (cdr bitsa) (cdr bitsb) (cadr sum)))
            ;and put the result part of the sum infront of the rest.
            ))
    ))
;defines a function to count the length of a list, because this was originaly in r5rs
(define (count ls)
  (define (count-i ls a)
    (if (null? ls)
        a
        (count-i (cdr ls) (+ a 1))
        ))
  (count-i ls 0))
;define bitwise not
(define (lsNot bits)
  (define (lsNoti bits res)
    (if (null? bits)
        res
        (lsNoti (cdr bits) (append res (list (bitNot (car bits)))))
        ))
  (lsNoti bits '())
  )
;define bitsub
(define (bit-sub bitsa bitsb)
  (if (not (and (= (count bitsa) int-bits) (= (count bitsb) int-bits))) ;if either of them is not the right size
      (bit-sub (mkSize bitsa int-bits) (mkSize bitsb int-bits)) ;call myself again with them both the right size
       ;add bitsa with (not bitsb) and 1, make sure it is the right size
      (mkSize (bit-add (bit-add bitsa (mkSize '(1) int-bits)) (lsNot bitsb)) int-bits)
      ))
;gets the last item of a list
(define (last ls)
  (item (count ls) ls))
;gets the nth item of a list
(define (item n ls)
  (if (or (= n 1) (< n 1))
      (car ls)
      (item (- n 1) (cdr ls))
      ))
;define bitwise multiplication
(define (bit-mult bitsa bitsb)
  ;define a helper function so it can be iterative
  (define (bit-mult-iter bitsa bitsb res)
    (cond ((null? bitsb) res) ;if we are out of bits to multiply by, return the result
          ((= (last bitsb) 1) ;if the last bit is true
           (bit-mult-iter bitsa
                          (mkSize bitsb (- (count bitsb) 1)) (bit-add (l-shift res) bitsa))
           ;multiply bitsa by bitsb without the last bit and set the result to res plus 2 times itself
           )
          (else (bit-mult-iter bitsa (mkSize bitsb (- (count bitsb) 1)) (l-shift res))
                ;otherwise, multiply bitsa by bitsb without the last bit and set res to two times itself
                )
      ))
  ;call the helper function with res being a bunch of zeros
  (bit-mult-iter bitsa bitsb (mkSize '(0) int-bits)))
(define (l-shift bits)
  (mkSize (append '(0) bits) int-bits))
(define (r-shift bits)
  (mkSize (cdr bits) int-bits))
(define (bit-expt bitsa bitsb)
  (define (bit-expt-iter bitsa bitsb res)
    (cond ((null? bitsb) res)
          ((= (last bitsb) 1)
           (bit-expt-iter bitsa (mkSize bitsb (- (count bitsb) 1)) (bit-mult (bit-mult res res) bitsa)))
          (else (bit-expt-iter bitsa (mkSize bitsb (- (count bitsb) 1)) (bit-mult res res)))
      ))
  (bit-expt-iter bitsa bitsb (mkSize '(1) int-bits))
  )
(define (last-one bits)
  (define (last-one-helper bits n)
    (cond ((null? bits) n)
          ((= (car bits) 1) (last-one-helper (cdr bits) (- int-bits (count bits))))
          (else (last-one-helper (cdr bits) n))
      ))
  (+ (last-one-helper bits -2) 1)
  )
(define (b>= ba bb)
  (let ((bitsa (mkSize ba int-bits)) (bitsb (mkSize bb int-bits)))
    (cond ((and (equal? bitsa bitsb) (equal? bitsa (mkSize '(0) int-bits))) #t)
        ((= (last bitsa) (last bitsb))
         (b>= (mkSize (append '(0) bitsa) int-bits) (mkSize (append '(0) bitsb) int-bits)))
        (else (= (last bitsa) 1))
    )))
(define (bit-div bitsa bitsb)
  (define (devide-helper bitsa bitsb n res)
    (if (or (> n 0) (= n 0))
        (let ((to-test (insert n 0 bitsb int-bits)))
          (if (b>= bitsa to-test)
              (devide-helper (bit-sub bitsa to-test) bitsb (- n 1) (insert 1 1 res int-bits))
              (devide-helper bitsa bitsb (- n 1) (insert 1 0 res int-bits))
             )) 
        res
        )
    )
  (devide-helper (mkSize bitsa int-bits) (mkSize bitsb int-bits)
                 (- int-bits (last-one (mkSize bitsb int-bits)))
                 (mkSize '(0) int-bits)))
(define (bit-mod bitsa bitsb)
  (define (mod-helper bitsa bitsb n)
    (if (or (> n 0) (= n 0))
        (let ((to-test (insert n 0 bitsb int-bits)))
          (if (b>= bitsa to-test)
              (mod-helper (bit-sub bitsa to-test) bitsb (- n 1))
              (mod-helper bitsa bitsb (- n 1))
              ))
        bitsa))
  (mod-helper (mkSize bitsa int-bits) (mkSize bitsb int-bits)
                 (- int-bits (last-one (mkSize bitsb int-bits)))))
(define (insert n as ls size)
  (define (iter n as ls)
    (if (= n 0)
      ls
      (insert (- n 1) as (cons as ls) size)
      ))
  (mkSize (iter n as ls) size))
(define (info)
  (display "this is the info for the bitmath program \n
 Bits are structured as lists like this \'(1 0 1 0 0 1), \n
 but note that for integers the first bit in the list represents 2^0 and the second 2^1 and so on, so the bitstring above represents 37 \n
 bit-add is a procedure that will add two bitstrings representing integers \n
 bit-sub is a procedure that will subtract one bitstring representing an integer from another \n
 bit-mult is a procedure that will multiply two bitstrings representing integers \n
 bit-div is a procedure that will devide one bitstring by another
 bit-mod is a procedure that will return the remainder of one bitstring devided by another"))
