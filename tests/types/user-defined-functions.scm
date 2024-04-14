(def square ((x . int)) -> int
    (* x x))

(square 5)

(def square ((x int)) -> int
    (* x x))

(square 5)

(def pi () -> float
    3.1415926535)

(pi)

(var value 100)

(def calc ((x int) (y int)) -> |fn(int -> int)|
    (begin
        (var z (+ x y))

        (def inner ((foo int)) -> int
            (+ (+ foo z) value))

        inner
    ))

(var fn (calc 10 20))

(fn 30)

(def factorial ((x int)) -> int
    (if (= x 1)
    1
    (* x (factorial (- x 1)))))

(factorial 5)
