;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ta-solver-starter-3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; ta-solver-starter.rkt



;  PROBLEM 1:
;
;  Consider a social network similar to Twitter called Chirper. Each user has a name, a note about
;  whether or not they are a verified user, and follows some number of people.
;
;  Design a data definition for Chirper, including a template that is tail recursive and avoids
;  cycles.
;
;  Then design a function called most-followers which determines which user in a Chirper Network is
;  followed by the most people.
;

;; ---------------------------
;; data definitions:

(define-struct user (name verified? follows))
;; User is (make-user String Boolean (listof user))
;; interp. a user on the Chirper network with name, whether they are verified or not,
;;         and users this user follows
(define USER1 (make-user "Ashli" true  empty))
(define USER2 (make-user "Obama" false (list USER1)))

(define (fn-for-user user)
  ;; todo is (listof User); Worklist accumulator
  (local [(define (fn-for-user user todo)
            (fn-for-lou (append (user-follows user) todo))) ; (user-name user), (user-verified? user)

          (define (fn-for-lou todo)
            (cond [(empty? todo) (...)]
                  [else
                   (fn-for-lou (first todo) (rest todo))]))]
    (fn-for-user user empty)))

;; Chirper is (listof User)
;; interp. a Chirper network, list of all users on the network
(define CHIRPER1 empty)
(define CHIRPER2 (shared ((-A- (make-user "A" false empty))
                          (-B- (make-user "B" false (list -A- -C-)))
                          (-C- (make-user "C" false (list -A-)))
                          (-D- (make-user "D" false (list -A- -B- -C- -D-))))
                   (list -A- -B- -C- -D-)))
(define CHIRPER3 (shared ((-A- (make-user "A" false (list -A- -C-)))
                          (-B- (make-user "B" false (list -A- -C-)))
                          (-C- (make-user "C" false (list -A-)))
                          (-D- (make-user "D" false (list -B- -C- -D-)))
                          (-E- (make-user "E" false (list -C-))))
                   (list -A- -B- -C- -D- -E-)))

(define (fn-for-chirper ch)
  ;; todo is (listof User); worklist accumulator
  ;; rsf is X; result so far
  ;; visited is (listof User); list of users already visited
  (local [(define (fn-for-chirper todo rsf visited)
            (cond [(empty? todo) (... rsf)]
                  [(member (first todo) visited) (fn-for-chirper (rest todo)
                                                                 rsf
                                                                 visited)]
                  [else
                   (fn-for-chirper (rest todo)
                                   (fn-for-user (first todo))
                                   (cons (first todo) visited))]))]
    (fn-for-chirper ch ... empty)))

;; --------------------------
;; functions:

;  Then design a function called most-followers which determines which user in a Chirper Network is
;  followed by the most people.

;; Chirper -> User
;; produce the user who is being followed by the most users on the given network
;; produces (make-user "" false empty) if no one is being followed or the list is empty
(check-expect (most-followers CHIRPER2) (first CHIRPER2))
(check-expect (most-followers CHIRPER3) (third CHIRPER3))
#;
(define (most-followers ch) (make-user "" false empty)) ;stub

(define (most-followers ch)
  ;; todo is (listof User); worklist accumulator
  ;; rsf is (listof UserNFollows); result so far
  ;; visited is (listof User); list of users already visited
  
  (local [;; UserNFollows is (make-unf User Natural)
          ;; the user and how many people follow them for worklist
          (define-struct unf (u f))

          (define (fn-for-chirper todo rsf visited)  ;; (listof User) (listof UserNFollows) (listof User) -> (listof UserNFollows)
            (cond [(empty? todo) rsf]
                  [(member (first todo) visited) (fn-for-chirper (rest todo)
                                                                 rsf
                                                                 visited)]
                  [else
                   (fn-for-chirper (rest todo)
                                   (merge-follows rsf
                                                  (user-follows (first todo)))
                                   (cons (first todo) visited))]))

          (define (merge-follows rsf lou)     ;; (listof UserNFollows)(listof User) -> (listof UserNFollows)
            (foldr merge-follows-fn rsf lou))

          (define (merge-follows-fn u lounf)  ;; User (listof UserNFollows) -> (listof UserNFollows)
            (cond [(empty? lounf) (cons (make-unf u 1) empty)]
                  [((lambda (u1 u2) (string=? (user-name u1) (user-name u2))) u (unf-u (first lounf)))
                   (cons (make-unf (unf-u (first lounf))
                                   (add1 (unf-f (first lounf))))
                         (rest lounf))]
                  [else
                   (cons (first lounf)
                         (merge-follows-fn u (rest lounf)))]))

          (define (get-most-followed lounf)  ;; (listof UserNFollows) -> User
            (unf-u (foldr (lambda (u1 u2)
                            (if (> (unf-f u1) (unf-f u2))
                                u1
                                u2))
                          (first lounf)
                          (rest lounf))))]
    
    (get-most-followed (fn-for-chirper ch empty empty))))


;  PROBLEM 2:
;
;  In UBC's version of How to Code, there are often more than 800 students taking
;  the course in any given semester, meaning there are often over 40 Teaching Assistants.
;
;  Designing a schedule for them by hand is hard work - luckily we've learned enough now to write
;  a program to do it for us!
;
;  Below are some data definitions for a simplified version of a TA schedule. There are some
;  number of slots that must be filled, each represented by a natural number. Each TA is
;  available for some of these slots, and has a maximum number of shifts they can work.
;
;  Design a search program that consumes a list of TAs and a list of Slots, and produces one
;  valid schedule where each Slot is assigned to a TA, and no TA is working more than their
;  maximum shifts. If no such schedules exist, produce false.
;
;  You should supplement the given check-expects and remember to follow the recipe!



;; Slot is Natural
;; interp. each TA slot has a number, is the same length, and none overlap

(define-struct ta (name max avail))
;; TA is (make-ta String Natural (listof Slot))
;; interp. the TA's name, number of slots they can work, and slots they're available for

(define SOBA (make-ta "Soba" 2 (list 1 3)))
(define UDON (make-ta "Udon" 1 (list 3 4)))
(define RAMEN (make-ta "Ramen" 1 (list 2)))

(define NOODLE-TAs (list SOBA UDON RAMEN))



(define-struct assignment (ta slot))
;; Assignment is (make-assignment TA Slot)
;; interp. the TA is assigned to work the slot

;; Schedule is (listof Assignment)


;; ============================= FUNCTIONS


;; (listof TA) (listof Slot) -> Schedule or false
;; produce valid schedule given TAs and Slots; false if impossible

(check-expect (schedule-tas empty       empty)      empty)
(check-expect (schedule-tas empty       (list 1 2)) false)
(check-expect (schedule-tas (list SOBA) empty)      empty)

(check-expect (schedule-tas (list SOBA) (list 1))   (list (make-assignment SOBA 1)))
(check-expect (schedule-tas (list SOBA) (list 2))   false)
(check-expect (schedule-tas (list SOBA) (list 1 3)) (list (make-assignment SOBA 1)
                                                          (make-assignment SOBA 3)))

(check-expect (schedule-tas NOODLE-TAs (list 1 2 3 4))
              (list
               (make-assignment RAMEN 2)
               (make-assignment SOBA 3)
               (make-assignment SOBA 1)
               (make-assignment UDON 4)))

(check-expect (schedule-tas NOODLE-TAs (list 1 2 3 4 5)) false)

#;
(define (schedule-tas tas slots) empty) ;stub

(define (schedule-tas tas slots)
  (local [(define (schedule-tas-losch losch rsf recursions)
            ;; rsf is (listof Schedule); result so far
            ;; recursions is Natural; count up of how many times this function has recursed
            (cond [(empty? losch) (if (< (add1 recursions) (length slots))
                                      (schedule-tas-losch (filter-duplicates rsf)
                                                          empty
                                                          (add1 recursions))
                                      rsf)]
                  [else
                   (schedule-tas-losch (rest losch)
                                       (append rsf
                                               (fill-slots tas
                                                           slots
                                                           (first losch)))
                                       recursions)]))
          
          (define (filter-duplicates losch) ;; (listof Schedule) -> (listof Schedule)
            (local [(define (filter-duplicates losch seen rsf)
                      ;; seen is (listof Schedule); schedules seen so far
                      ;; rsf is (listof Schedule); result so far
                      (cond [(empty? losch) rsf]
                            [else
                             (if (member (first losch)
                                         seen)
                                 (filter-duplicates (rest losch) seen rsf)
                                 (filter-duplicates (rest losch)
                                                    (cons (first losch)
                                                          seen)
                                                    (cons (first losch)
                                                          rsf)))]))]
              (filter-duplicates losch empty empty)))
          
          (define (find-valid losch)
            (cond [(empty? losch) false]
                  [else
                   (if (valid? (first losch))
                       (first losch)
                       (find-valid (rest losch)))]))
          
          (define (valid? schedule)
            (= (length schedule)
               (length slots)))]
    
    (if (empty? slots)
        empty
        (find-valid (schedule-tas-losch (list empty) empty 0)))))

;; (listof TA) (listof Slot) Schedule -> (listof Schedule)
;; produces a list containing the given Schedule with every valid assignment for one slot
(check-expect (fill-slots empty empty empty)       empty)
(check-expect (fill-slots (list SOBA) empty empty) empty)

(check-expect (fill-slots (list SOBA) (list 1) empty)            (list (list (make-assignment SOBA 1))))
(check-expect (fill-slots (list SOBA) (list 1 2 3) empty)        (list (list (make-assignment SOBA 1))
                                                                       (list (make-assignment SOBA 3))))
(check-expect (fill-slots (list SOBA UDON) (list 1 2 3 4) empty) (list (list (make-assignment SOBA 1))
                                                                       (list (make-assignment SOBA 3))
                                                                       (list (make-assignment UDON 3))
                                                                       (list (make-assignment UDON 4))))
#;
(define (fill-slots tas slots schedule) empty) ;stub

(define (fill-slots tas slots schedule)
  (local [(define (fill-slots slots rsf)
            ;; rsf is (listof Schedule); result so far
            (cond [(empty? slots) rsf]
                  [else
                   (fill-slots (rest slots)
                               (append rsf
                                       (fill-slot tas
                                                  (first slots)
                                                  schedule)))]))]
    (fill-slots slots empty)))




;; (listof TA) Slot Schedule -> (listof Schedule)
;; produce each valid combination of assignments with given slot
(check-expect (fill-slot empty       1 empty) empty)
(check-expect (fill-slot (list SOBA) 2 empty) empty)

(check-expect (fill-slot empty       1 (list (make-assignment SOBA 1))) (list (list (make-assignment SOBA 1))))
(check-expect (fill-slot (list SOBA) 2 (list (make-assignment SOBA 1))) (list (list (make-assignment SOBA 1))))
(check-expect (fill-slot (list UDON) 4 (list (make-assignment UDON 3))) (list (list (make-assignment UDON 3))))
(check-expect (fill-slot (list SOBA) 3 (list (make-assignment UDON 3))) (list (list (make-assignment UDON 3))))

(check-expect (fill-slot (list SOBA)      1 empty) (list (list (make-assignment SOBA 1))))
(check-expect (fill-slot (list SOBA UDON) 1 empty) (list (list (make-assignment SOBA 1))))

(check-expect (fill-slot (list SOBA UDON) 3 empty) (list (list (make-assignment SOBA 3))
                                                         (list (make-assignment UDON 3))))
#;
(define (fill-slot tas slot schedule) empty) ;stub

(define (fill-slot tas slot schedule)
  (local [(define (fill-slot tas)
            (if (not (empty? schedule))
                (cons schedule (filter-same (map assign-ta tas)))
                (filter-same (map assign-ta tas))))

          (define (assign-ta ta) ;; TA -> Schedule
            (if (and (member slot (ta-avail ta))
                     (not (too-many? ta))
                     (not (slot-occupied? schedule))
                     (not (member (make-assignment ta slot)
                                  schedule)))
                (cons (make-assignment ta slot)
                      schedule)
                schedule))

          (define (filter-same losch) ;; (listof Schedule) -> (listof Schedule)
            (filter (lambda (schedule-1)
                      (not (equal? schedule-1 schedule)))
                    losch))

          (define (too-many? ta) ;; TA -> Boolean
            (>=
             (length
              (filter (lambda (assignment)
                        (equal? ta (assignment-ta assignment)))
                      schedule))
             (ta-max ta)))

          (define (slot-occupied? schedule) ;; Schedule -> Boolean
            (ormap (lambda (assignment)
                     (= slot (assignment-slot assignment))) schedule))]
    
    (fill-slot tas)))


