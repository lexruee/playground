points = [ [x, y] | x <- [0..5], y <- [6..11] ]
sq z = z * z
squares = [ sq x | x <- [0..10] ]

xs = 1 : 2 : 3 : 4  : []
boolean_value = xs == [1,2,3,4]


ys = [ 1 | _ <- [1..10]]
