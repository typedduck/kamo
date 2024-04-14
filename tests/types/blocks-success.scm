; 1: empty block
(begin)

; 2: block with one expression
(begin 1)

; 3: block with two declarations in local scope
(begin
    (var x 10)
    (var y 20)
    (+ (* x 10) y))

; 4: block with two declarations in nested scope
(begin
    (var x 10)

    (begin
        (var y 20)
        (+ x y)))

; 5: block with two declarations in nested scope and one assignment
(begin
    (var x 10)

    (begin
        (var y 20)
        (set x (+ x y))))

; 6: declaration in global scope
(var x 10)

; 7: declaration in global scope
(var y 20)

; 8: access to global variables
(+ (* x 10) y)
