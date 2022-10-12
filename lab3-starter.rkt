;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab3-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Consider the following data definitions & interpretations...


(define-struct address [num st city us-state zip])

; An Address is a (make-address Nat String String String Nat)
; - num is the number of the building on the street
; - st is the name of the street
; - city is the city the building is in
; - us-state is the state the city is in
; - zip is the zipcode of the building
; Interpretation: a US address

(define ADDRESS-1 (make-address 21 "Forsyth" "Boston" "Massachusetts" 02215))
(define ADDRESS-2 (make-address 1600 "Amphitheatre" "Mountain View" "California" 94043))
(define ADDRESS-3 (make-address 1762 "W 101" "New York City" "New York" 10021))
(define ADDRESS-4 (make-address 395 "New Haven" "Peterborough" "Massachusetts" 03458))
(define ADDRESS-5 (make-address 1214 "Oak" "Los Angeles" "California" 90291))

(define (address-temp address)
  (... (address-num address) ...
       (address-st address) ...
       (address-city address) ...
       (address-us-state address) ...
       (address-zip address) ...))


(define-struct student [first last nuid local perm])

; An NUStudent is a (make-student String String PositiveNumber Address Address)
; - first is the student's first name
; - last is the student's last name
; - nuid is the student's NUID
; - local is the student's local address
; - perm is the student's permanent address
; Interpretation: a Northeastern student

(define STU-1 (make-student "Charlie" "Miller" 714954 "21 Forsyth" "1221 Forest Hill"))
(define STU-2 (make-student "Alexander" "Washington" 708313 "7 Speare" "8357 Oak Grove"))
(define STU-3 (make-student "Trisha" "Parkinson" 021539 "1155 Tremont" "0620 Cobblestone"))
(define STU-4 (make-student "Aimee" "Takhat" 618092 "144 Hemenway" "87 Cork"))
(define STU-5 (make-student "Sasha" "Clark" 538201 "780 Colombus" "567 N Main"))


(define (student-temp student)
  (... (student-first student) ...
       (student-last student) ...
       (student-nuid student) ...
       (student-local student) ...
       (student-perm student) ...))

; TODO 1/3: complete the data design recipe for Address and NUStudent




; TODO 2/3: Design the function student-email which takes an NUStudent and
;           produces a string representing that student’s email address.
;           For simplicity we will say that a student’s email address is always
;           their last name (all lowercase),  followed by a period, followed
;           by the first initial of their first name (also lowercase; you can
;           assume this exists), and finished with "@northeastern.edu".

; student-email : Student –> String
; Takes a student's name and creates an email address for them.

(check-expect (student-email STU-1) "miller.charlie@northeastern.edu")
(check-expect (student-email STU-3) "parkinson.trisha@northeastern.edu")
(check-expect (student-email STU-4) "takhat.aimee@northeastern.edu")

(define (student-email student)
  (string-append (string-downcase(student-last student)) "."
                 (string-downcase(student-first student))
                 "@northeastern.edu"))



; TODO 3/3: Design the function update-perm-zipcode which takes an NUStudent
;           and a natural number, representing the new zip code of the person,
;           and updates their permanent address to have that zip code.
;
;           Be sure to follow the template!

; update-perm-zipcode : Student Number –> Student
; Updates the permanent address of a student to have a new zip code.

(check-expect (update-perm-zipcode STU-1 874531) (make-student (student-first STU-1)
                                                               (student-last STU-1)
                                                               (student-nuid STU-1)
                                                               (student-local STU-1)
                                                               874531))
(check-expect (update-perm-zipcode STU-2 000251) (make-student (student-first STU-2)
                                                               (student-last STU-2)
                                                               (student-nuid STU-2)
                                                               (student-local STU-2)
                                                               000251))
(check-expect (update-perm-zipcode STU-4 193619) (make-student (student-first STU-4)
                                                               (student-last STU-4)
                                                               (student-nuid STU-4)
                                                               (student-local STU-4)
                                                               193619))

(define (update-perm-zipcode student number)
  (make-student (student-first student)
                (student-last student)
                (student-nuid student)
                (student-local student)
                number))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Note: to receive full credit, submit as much as you complete - you do NOT
;       have to finish all parts in lab.


; You are to design a program text-mover to display and manipulate text on a
; background. Your program should accept some phrase to show, as well as initial
; location and color (we only support three: red, black, or purple) - you should
; then display the phrase on the screen as described.

; When the user presses a mouse button, the program should move the text to the
; location that they clicked. When the user presses any key on the keyboard, the
; program should rotate colors.

; Here is our suggested plan for this program...

; 1. Design the text-mover function - think through the arguments to the
;    function, how you will represent the world state, and what handlers
;    you need to support.
;
;    - Hint A: since your state has multiple parts that change, you'll need a
;              structure to hold them, but the parts themselves might also be new.
;    - Hint B: you've been provided some data definitions below that will be quite
;              useful.

; 2. Finish designing the data from #1; think ahead to make examples that are
;    useful for testing such operations as changing location and color.

; 3. Design your to-draw handler, making use of the template(s) you 
;    designed in #2.

; 4. Design your remaining handler(s), again following the appropriate template(s).
;
;    - Hint #1: for the mouse, you'll want to respond only to the "button-up"
;               event, which you can check using the mouse=? function. Here's code
;               to get you started...


(define SCENE (empty-scene 500 250))

(define (mouse-handler state x y me)
  (if (mouse=? me "button-up")
      ...
      ...))

;    - Hint #2: make sure to follow your templates, which may involve breaking 
;               the handlers into helper functions.


; TODO 1/1: Design the text-mover World program!


; A Position is a (make-posn Real Real)
; Interpretation: a 2D location

(define PS1 (make-posn 50 50))
(define PS2 (make-posn 20 140))
(define PS3 (make-posn 50 60))
(define PS4 (make-posn 0 0))
(define PS5 (make-posn 75 125))

(define (aposn posn-x posn-y)
  (... (posn-x) ...
       (posn-y) ...))


; A RedBlackPurple (RBP) is one of:
; - "red"
; - "black"
; - "purple"
; Interpretation: available font colors

(define RED "red")
(define BLACK "black")
(define PURPLE "purple")

(define (color-temp state)
  (... (equal? (tm-col state) RED) ...
       (equal? (tm-col state) BLACK) ...
       (equal? (tm-col state) PURPLE) ... ))

(define-struct tm [str pos col])

; A TextMover (TM) is a (make-tm String Position RBP)
; - str is the text to be displayed
; - pos is the location of the text
; - col is the color of the text
; Interpretation: all the information needed for the text-mover program.

(define TM-1 (make-tm "Hello World" PS1 "purple"))
(define TM-2 (make-tm ":D" PS2 "red"))
(define TM-3 (make-tm "The quick brown fox" PS3 "black"))
(define TM-4 (make-tm "The lazy dog" PS4 "purple"))


(define (tm-temp state)
  (... (tm-str state) ...
       (tm-pos state) ...
       (tm-col state) ... ))


; draw-tm : TM –> Image
; Draws the text on top of the scene.
(define (draw-tm tm)
  (place-image (text (tm-str tm) 30 (tm-col tm))
               (posn-x (tm-pos tm)) (posn-y (tm-pos tm)) SCENE))

; mouse-tm : TM Number Number MouseEvent –> TM
; Moves the text depending on the position of the mouse click.
(define (mouse-tm state x y me)
  (if (mouse=? me "button-up")
      (make-tm (tm-str state) (make-posn x y) (tm-col state))
      state))

; text-mover : TM –> Image
; Draws the image and moves it with the handlers.
(define (text-mover tm)
  (big-bang (make-tm (tm-str tm) (tm-pos tm) (tm-col tm))
    [to-draw draw-tm]
    [on-mouse mouse-tm]))


