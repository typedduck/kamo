(type number int)

(type ID number)

(type Index ID)

(def square ((x number)) -> number (* x x))

(square 2)

(def promote ((userID ID)) -> ID (+ 1 userID))

(promote 1)

(var (x Index) 1)

x
