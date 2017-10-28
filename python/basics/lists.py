#!/usr/bin/env python

from functools import reduce

squares = [x * x for x in range(0,10)]

for square in squares:
    print(square)

a = [1,2,3]
b = [4,5,6]
c = a + b
print(c)

print(sum(c))
print([x for x in filter(lambda v: v >= 4, c)])
print([x for x in c if x >= 4])

zipped = zip(range(0,10), squares)
print(list(zipped))


oneToTen = list(range(0, 10))
print(oneToTen)
doubles = map(lambda x: x * 2, oneToTen)
print(list(doubles))

mysum = reduce(lambda x, y: x + y, range(0, 10))
print(mysum)

