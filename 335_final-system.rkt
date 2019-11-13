(#%require 2htdp/image) ;only used to show image of a graph
;(define graph1 '((a (b d)) (b (a c)) (c (b d)) (d (a c))))
;(define graph2 '((a (b c)) (b (a c d)) (c (a b d e f)) (d (b c)) (e (c d)) (f (c e))))
;(define graph3 '((a (b c e f)) (b (a c d f)) (c (a b d e)) (d (c e)) (e (a c d f)) (f (a b d e))))
(define graph4 '((a (b d)) (b (a c f)) (c (b d e f)) (d (a c e)) (e (c d f)) (f (a b e))))
;(define graph5 '((a (b f)) (b (a c f)) (c (b d e)) (d (c e)) (e (c d f)) (f (a b e))))
(define graph1 '((a (b f)) (b (a g)) (c (g h)) (d (h e)) (e (d h)) (f (a g)) (g (b c h i)) (h (c d e g i j)) (i (g h)) (j (h))))
(define graph2 '((a (b c d e)) (b (a c d e)) (c (a b d e)) (d (a b c e)) (e (a b c d))))
(define graph3 '((1 (2 3 4)) (2 (1 4 5)) (3 (1 6)) (4 (1 2 3 5 6 7)) (5 (2 4 7)) (6 (3 4 7)) (7 (4 5 6))))
(define graph5 '((a (b c d e)) (b (a c d f)) (c (a b e f)) (d (a b e f)) (e (a c d f)) (f (b c d e))))
(define get-node caar) ; (caar graph) -> a
(define adj-nodes cadar) ; (cadar graph) -> (b f) - (car (adj-nodes graph)) = b | (cadr (adj-nodes graph)) = f
(define color '(red green blue black))
;=============================================================================================
;=============================================================================================
(define atom?
  (lambda (x)
    (and (not (null? x)) 
	 (not (pair? x)))))

(define (replacelst lst old new) ;replacing a list with a new list. MIGHT NOT NEED THIS FUNCTION
  (cond ((null? lst) '())
        ((equal? (car lst) old) (cons new (cdr lst)))
        (else (cons (car lst) (subst* (cdr lst) old new)))))

(define (replace lst node color) ;replaces the node's color
  (cond ((equal? (caar lst) node) (cons (list node color) (cdr lst)))
        (else (cons (car lst) (replace (cdr lst) node color)))))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))

;(define (replace lst node color) ;replaces the node's color
;  (cond ((equal? (caar lst) node) (cons (list node 'green) (cdr lst)))
;        (else (cons (car lst) (replace (cdr lst) node color)))))

;=============================================================================================
;=============================================================================================
(define (traverse-graph graph)
  (define (aux graph result)
    (cond ((null? graph) (reverse result))
          (else (aux (cdr graph) (cons (list (get-node graph) '-) result)))))
  (aux graph '()))
;=============================================================================================
;=============================================================================================
(define colored-graph1 (traverse-graph graph1))
(define colored-graph2 (traverse-graph graph2))
(define colored-graph3 (traverse-graph graph3))
(define colored-graph4 (traverse-graph graph4))
(define colored-graph5 (traverse-graph graph5))
;traverse ((a -) (b -) (c -) (d -)) ...
;change function up above to match the graph. graph1...graph2... and so on

;=============================================================================================
;=============================================================================================

(define (get-color node color-graph) ;returns the color of the node you inputted.
  (cond ((null? color-graph) '())
        ((eq? node (get-node color-graph)) (cadar color-graph))
        (else (get-color node (cdr color-graph)))))

(define (check-adj-node node-list color-graph) ;returns the correct color for the current node
  (define (aux node-list color)
    (cond ((null? node-list) (car color))
          ((eq? (get-color (car node-list) color-graph) '-) (aux (cdr node-list) color))
          ((eq? (get-color (car node-list) color-graph) 'red) (aux (cdr node-list) (rember 'red color)))
          ((eq? (get-color (car node-list) color-graph) 'green) (aux (cdr node-list) (rember 'green color)))
          ((eq? (get-color (car node-list) color-graph) 'blue) (aux (cdr node-list) (rember 'blue color)))
          (else (aux (cdr node-list) color))))
  (aux node-list color))

(define (checker-helper graph color-graph replaced-node)
  (checker graph replaced-node))

(define (checker graph color-graph)
  (cond ((null? graph) (display color-graph))
        ((eq? (check-adj-node (adj-nodes graph) color-graph) 'red)
         (checker-helper (cdr graph) color-graph (replace color-graph (get-node graph) 'red)))
        ((eq? (check-adj-node (adj-nodes graph) color-graph) 'green)
         (checker-helper (cdr graph) color-graph (replace color-graph (get-node graph) 'green)))
        ((eq? (check-adj-node (adj-nodes graph) color-graph) 'blue)
         (checker-helper (cdr graph) color-graph (replace color-graph (get-node graph) 'blue)))
        ((eq? (check-adj-node (adj-nodes graph) color-graph) 'black)
         (checker-helper (cdr graph) color-graph (replace color-graph (get-node graph) 'black)))
        (else (display "ERROR"))))

;=============================================================================================
;=============================================================================================
(define (color-solver graph color colored-graph)
  (define improved-graph (checker graph colored-graph))
  improved-graph)

(bitmap/file "G1.jpg")
(newline)
(color-solver graph1 color colored-graph1)
(newline)
(bitmap/file "G2.jpg")
(newline)
(color-solver graph2 color colored-graph2)
(newline)
(bitmap/file "G3.jpg")
(newline)
(color-solver graph3 color colored-graph3)
(newline)
(bitmap/file "G4.jpg")
(color-solver graph4 color colored-graph4)
(newline)
(bitmap/file "6.2.jpg")
(newline)
(color-solver graph5 color colored-graph5)
;replace graph to graph1... graph2... so on.









