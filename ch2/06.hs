zero :: t -> t1 -> t1
zero f x = x
-- zero = \f -> (\x -> x)

inc :: ((t -> t1) -> t2 -> t) -> (t -> t1) -> t2 -> t1
inc n f x = f ((n f) x)
-- inc n = \f -> (\x -> (f ((n f) x)))

one :: (t -> t1) -> t -> t1
one f x = f x
-- one = \f -> (\x -> (f x))

two :: (t -> t) -> t -> t
two f x = f (f x)
-- two = \f -> \x -> (f (f x))

three :: (t-> t) -> t -> t
three f x = f (f (f x))
-- three = \f -> \x -> (f (f (f x)))

plus :: (t2 -> t -> t1) -> (t2 -> t3 -> t) -> t2 -> t3 -> t1
plus m n f x = m f (n f x)
-- plus m n = \f -> (\x -> (m f (n f x)))

mul :: (t -> t2 -> t3) -> (t1 -> t) -> t1 -> t2 -> t3
mul m n f x = m (n f) x
-- mul m n = \f -> (\x -> (m (n f) x))

pure_zero = 0
pure_inc x = x + 1

-- pure_zero = ""
-- pure_inc x = "a" ++ x

main :: IO ()
main = do
        print $ zero pure_inc pure_zero
        print $ two pure_inc pure_zero
        print $ (inc two) pure_inc pure_zero
        print $ (plus two three) pure_inc pure_zero
        print $ (mul two three) pure_inc pure_zero
        print $ (mul three three) pure_inc pure_zero
