Defining number types in haskell is fun. [Let's represent a number by it's logarithm!](
https://en.wikipedia.org/wiki/Logarithmic_number_system)

```
{-# LANGUAGE TypeApplications #-}
import Data.Int

d :: Num a => a
d = 8

newtype L8 = L8 Int8
```

Each number is represented by an 8 bit int, `x`. This integer represents a real number `y = 2^(x/d)`. Another view is that we're storing a fixed point number `z = x/d` and that the number we're representing the same number `y = 2^z`.

This can represent some positive rational numbers. It's a nice way of learning some about floating point numbers because the errors are a bit more regular than floats. And with only 8 bits we will get a lot of error! Interestingly we don't have 0. In floats 0 is a special representation, similar to NaN or infinities. We also dont have negative numbers, we could add that via a sign bit.

Let's add literals for our numbers.

```
instance Fractional L8 where
    fromRational = l8_fromRational
    (/) = l8_div
```

The `Fractional` typeclass handles literals like `0.1` via `fromRational`. It also defines division and `recip`.

Sidenote, the number classes are a bit of a mess. But they are still fun to play around with.

The transormation from a Rational to our representation looks a bit like this: `y = 2^(x/d) => x = floor (log2 y * d)`.

```
l8_fromRational :: Rational -> L8
l8_fromRational = L8 (satFloor (d * log2 (fromRational x)))
```

The `@Double` is a [type application](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-TypeApplications). None of the type applications are necessary in this code but we will use them to point out which type is used in some general type.

We start by making the Rational into a Double. There is some inaccuracy there but a Double can represent all numbers that a L8 can so in the end it shouldnt be a problem. We then do the arithmetic for fixed point and logarithm. Lastly we turn the double into a Int8 by a saturating floor function.

We made use of some helper functions

```
log2 :: Floating a => a -> a
log2 = logBase 2

viaInteger :: (Integral a, Num c) => a -> c
viaInteger = fromInteger . toInteger -- aka fromIntegral

instance Bounded L8 where
    -- the smallest 2^x is the one with the smallest x
    minBound = L8 minBound
    maxBound = L8 maxBound

-- saturating floor function
-- strange sidenote: floor (log2 0) == (0 :: Int)
satFloor :: (RealFrac a, Bounded b, Integral b)
         => a -> b
satFloor x = y
    where y  = floor . min hi . max lo $ x
          lo = viaInteger (minBound `asTypeOf` y)
          hi = viaInteger (maxBound `asTypeOf` y)
```

The saturating part means that `-1`, or another number less than what we can represent, will result in the smallest number representable and the same for numbers bigger than what we can represent. The choice of floor instead of round and to saturate instead of error is mostly arbitrary.

Now let's define Show which will show our literals to us after they've been converted.

```
viaRat :: (Fractional c, Real a) => a -> c
viaRat = fromRational . toRational -- realToFrac

instance Real L8 where
    toRational (L8 x) = viaRat (2**(viaRat x / d))

instance Show L8 where
    show = show @Double . viaRat
```

Allright let's test it out

```
λ> 0 :: L8
1.52587890625e-5
λ> 1 :: L8
1.0
λ> 2 :: L8
2.0
λ> 3 :: L8
2.8284271247461903
λ> 0.5 :: L8
0.5
λ> 0.1 :: L8
9.63881765879963e-2
```

Nice ... that's an interesting outcome. What's the largest number we can represent?

And finally we define some arithmetic.

```
unwrap f (L8 x) = L8 (f x)
unwrap' f (L8 x) = f x
unwrap2 f (L8 x) (L8 y) = L8 (f x y)
unwrap2' f (L8 x) (L8 y) = f x y

l8_div :: L8 -> L8 -> L8 -- this was used in Fractional
l8_div = unwrap2 (-)

instance Num L8 where
    fromInteger  = viaRat
    -- adding logarithmic numbers is hard so we let Double deal with it
    x + y = viaRat (viaRat x + viaRat y :: Double)
    x - y = viaRat (viaRat x - viaRat y :: Double)
    -- 2^x * 2^y = 2^(x+y)
    (*) = unwrap2 (+)
    abs = id
    signum = const 1

-- compare x y == compare (2^x) (2^y)
instance Eq L8 where
    (==) = unwrap2' (==)
instance Ord L8 where
    compare = unwrap2' compare

-- TODO steps of +1
instance Enum L8 where
    fromEnum (L8 x) = fromEnum x
    toEnum   x = L8 (toEnum   x)
```
