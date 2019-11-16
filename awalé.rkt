;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname awalé) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define liste-joueur '(4 4 4 4 4 4))
(define liste-ordinateur '(4 4 4 4 4 4))
(define score-ordinateur 0)
(define score-joueur 0)
(define trou-aleatoire 0)


(define (nombre-bille liste trou)
  (if (= trou 1)
      (car liste)
      (nombre-bille (cdr liste) (- trou 1))
  )
)

(define (choisir-trou-aleatoire)
  (+ (random 5) 1)
)

(define (somme-liste liste)
  (+ (nombre-bille liste 1) (nombre-bille liste 2) (nombre-bille liste 3) (nombre-bille liste 4) (nombre-bille liste 5) (nombre-bille liste 6))
)

(define (maj-score-joueur score)
  (set! score-joueur (+ score-joueur score))
)

(define (maj-score-ordinateur score)
  (set! score-ordinateur (+ score-ordinateur score))
)

(define (fin?)
  (if (or (= (somme-liste liste-joueur) 0) (= (somme-liste liste-ordinateur) 0))
      (begin
        (display "here")
      true
      )
      false
  )
)

(define (add liste trou)
  (if (= trou 1)
      (cons (+ (car liste) 1) (cdr liste) )
      (cons (car liste) (add (cdr liste) (- trou 1)))
  )
)

(define (liste+1-joueur billes trou)
  (if (or (= billes 0) (= trou 7))
      (void)
      (begin
        (set! liste-joueur (add liste-joueur trou))
        (liste+1-joueur (- billes 1) (+ trou 1))
      )
  )
)
(define (liste+1-ordinateur billes trou)
  (if (or (= billes 0) (= trou 0))
      (void)
      (begin
        (set! liste-ordinateur (add liste-ordinateur trou))
        (liste+1-ordinateur (- billes 1) (- trou 1))
      )
  )
)

(define (maj-suite-joueur billes )
    (if (= billes 1)
       (maj-score-ordinateur 1)
       (if (<= billes 0)
           (void)
           (begin
           
               (liste+1-joueur billes 1)
               (maj-suite-ordinateur (- billes 6) )
               
           )
       )
   )
 )

(define (maj-suite-ordinateur billes)
  
  (if (= billes 1)
       (maj-score-joueur 1)
       (if (<= billes 0)
           (void)
           (begin
           
               (liste+1-ordinateur billes 6)
               (maj-suite-joueur (- billes 6) )
               
           )
       )
   )
)

(define c 0)


(define (annuler trou liste)
  (if (= trou 1)
      (cons 0 (cdr liste) )
      (cons (car liste)(annuler (- trou 1) (cdr liste)) )
      
  )
)


(define (maj-joueur trou)
  (begin
   (set! c (nombre-bille liste-joueur  trou))
   (display c)
   (set! liste-joueur (annuler trou liste-joueur))
   (liste+1-joueur c (+ trou 1))
   (if (> (- c (- 6 trou)) 0)
             (begin
               (display "restante: ")
              (display (- c (- 6 trou)))
              (newline)
             (maj-suite-ordinateur (- c (- 6 trou)) )
             )
             (void)
  )
)
  )

(define (maj-ordinateur trou)
  (begin
   (set! c (nombre-bille liste-ordinateur  trou))
   (set! liste-ordinateur (annuler trou liste-ordinateur))
   (liste+1-ordinateur c (- trou 1))
   (if (> (- c (- trou 1)) 0)
             (maj-suite-joueur (- c (- trou 1)) )
             (void)
  )
   )
)



(define (f trou)
    (if (= (nombre-bille liste-joueur  trou ) 1)
     (begin
      (maj-score-joueur (+ 1 (nombre-bille liste-ordinateur  trou )))
      (set! liste-ordinateur (annuler trou liste-ordinateur))
      (set! liste-joueur (annuler trou liste-joueur))
     )
     (void)
  )
 )


(define (maj-1-bille-joueur)
  (begin
  (f 1)
  (f 2)
  (f 3)
  (f 4)
  (f 5)
  (f 6))

)

(define (g trou)
  (begin
  ;(display (nombre-bille liste-ordinateur  trou ))
   (if (= (nombre-bille liste-ordinateur  trou ) 1)
     (begin
       ;(display "here")
       ;(newline)
      (maj-score-ordinateur (+ 1 (nombre-bille liste-joueur  trou )))
      (set! liste-ordinateur (annuler trou liste-ordinateur))
      (set! liste-joueur (annuler trou liste-joueur))
     )
     (void)
  ))
)
(define (maj-1-bille-ordinateur)
  (begin
    
 (g 1)
  (g 2)
  (g 3)
  ;(display "g")
  ;(newline)
  (g 4)
  (g 5)
  (g 6)
  )
)


(define (jouer trou)
  (begin
  (maj-joueur trou)
  (maj-1-bille-joueur)
  (write liste-ordinateur)
  (newline)
  (write liste-joueur)
  (newline)
  (display "score-ordi: ")
  
  (display score-ordinateur)
  (newline)
  (display "score-joueur: ")
  (display score-joueur)
  (newline)
    (if (eq? (fin?) true )
       (begin
      (if (= (somme-liste liste-ordinateur) 0)
          (maj-score-ordinateur (somme-liste liste-joueur))
      
       (maj-score-joueur (somme-liste liste-ordinateur))
      )
      (if (> score-ordinateur score-joueur)
          (display "ordi win!")
          
             (if (< score-ordinateur score-joueur)
                 (display "joueur win!")
              
             (display "tie!")
             )
           
       )
      )
       (void)
  )
  )
)

(define t 0)
(define (jouer-ordi)
  (begin
   (set! t (choisir-trou-aleatoire))
   (display "trou-aleatoire: ")
   (display t)
   (newline)
  (maj-ordinateur t)
  ;(display liste-ordinateur)
  (maj-1-bille-ordinateur)
  (display liste-ordinateur)
  (newline)
  (display liste-joueur)
  (newline)
  (write "score-ordi: ")
  (display score-ordinateur)
  (newline)
  (write "score-joueur: ")
  (display score-joueur)
  (newline)
      (if (eq? (fin?) true )
       (begin
      (if (= (somme-liste liste-ordinateur) 0)
          (maj-score-ordinateur (somme-liste liste-joueur))
      
       (maj-score-joueur (somme-liste liste-ordinateur))
      )
      (if (> score-ordinateur score-joueur)
          (display "ordi win!")
          
             (if (< score-ordinateur score-joueur)
                 (display "joueur win!")
              
             (display "tie!")
             )
           
       )
      )
       (void)
  )
  )
)




