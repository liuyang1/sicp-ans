-- Haksell is cooler than racket. :) Haha...
-- Maybe I need try typed-racket.
mycons :: a -> b -> (a -> b -> c) -> c
mycons a b = \f -> f a b

mycar :: ((b -> c -> b) -> a) -> a
mycar z = z (\a b -> a)

mycdr :: ((b -> c -> c) -> a) -> a
mycdr z = z (\a b -> b)
