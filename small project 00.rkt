#lang racket
(require test-engine/racket-tests)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CS 3180 Fall 2019 Small project 00
;; Trent J. M. Church
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Description
;;finds the max of two numbers
;;returns the max
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (max2 a b)
  (if (> b a) b a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Description
;;finds the max of N numbers
;;returns the max
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (max lst)
  (cond
    [(empty? lst) "Bad"]
    [(empty? (rest lst))(first lst)]
    [else (max2(first lst)(max(rest lst)))]
    )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Description
;;finds whether a or be comes find in a list
;;returns the True if a before b in the list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define(before-in-list? lst a b)
  (cond
    [(empty? lst) #F]
    [(equal? (first lst) b)#F]
    [(equal? (first lst) a)#T]
    [else (before-in-list? (rest lst) a b)]

   )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Description
;;finds the dot product of two vectors
;;returns the dot product
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define(dotProduct lsta lstb)
  (cond
    [(not(equal? (length lsta) (length lstb)))'*incompatible*]
    [(empty? lsta) 0]
    [else (+ (* (first lsta) (first lstb)) (dotProduct(rest lsta) (rest lstb)))]

   )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Description
;;removes all occurences of A in lst
;;returns new lst
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (removeARow lst a)
  (cond
  [(empty? lst)  lst]
  [(equal? (first lst) a)  (removeARow (rest lst) a)]
  [else (append (list (first lst))(removeARow (rest lst) a))]
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Description
;;removes all occurences of each character in lstb in lsta
;;returns new lsta
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (removeAll lsta lstb)
  (cond
    [(empty? lsta) lsta]
    [(empty? lstb) lsta]
    [else (removeAll (removeARow lsta (first lstb)) (rest lstb))]   
    
   )
  

  )
;true a before b
 (before-in-list? '(back in the ussr) 'in 'ussr)
;false b before a 
(before-in-list? '(back in the ussr) 'the 'back)
;false neither in list
(before-in-list? '(back in the ussr) 'then 'backer)
;false empty list
(before-in-list? '() 'the 'back)


;dot product of two empty vectors is 0
(dotProduct '() '())
;test vectors of different sizes B > A
(dotProduct '(1 3) '(4 5 6))
;test vectors of different sizes A > B
(dotProduct '(1 2 3) '(4 6))
;test vectors for correct answer
(dotProduct '(1 2 3) '(4 5 6))

; test empty first list
(removeAll '() '(d s a c a))
;test empty second list
(removeAll '(a b b c c d) '())
; test for correct answer
(removeAll '(a b b c c d) '(d s a c a))

(check-expect  '((before-in-list? '(back in the ussr) 'in 'ussr)) #t)
(check-expect  '((before-in-list? '(back in the ussr) 'the 'back)) #f)
(check-expect  '((before-in-list? '(back in the ussr) 'then 'backer)) #f)
(check-expect  '((before-in-list? '() 'the 'back)) #f)
(check-expect  '((dotProduct '() '())) 0)
(check-expect  '((dotProduct '(1 3) '(4 5 6))) "*incompatible*")
(check-expect  '((dotProduct '(1 2 3) '(4 6))) "*incompatible*")
(check-expect  '((dotProduct '(1 2 3) '(4 5 6))) 32)
(check-expect  '((removeAll '() '(d s a c a))) '())
(check-expect  '((removeAll '(a b b c c d) '())) '(a b b c c d))
(check-expect  '((removeAll '(a b b c c d) '(d s a c a))) '(b b))