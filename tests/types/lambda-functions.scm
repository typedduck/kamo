(lambda ((x int)) -> int (* x x))

(def onClick ((callback |fn(int -> int)|)) -> int
    (begin
        (var x 10)
        (var y 20)
        (callback (+ x y))))

(onClick (lambda ((data int)) -> int (* data 10)))

((lambda ((x int)) -> int (* x x)) 2)

(var square (lambda ((x int)) -> int (* x x)))

(square 2)
