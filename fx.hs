f' x = x + x

bmiTell :: (RealFrac a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "underweight"
    | bmi <= normal = "normal"
    | bmi <= fat = "fatty"
    | otherwise = "whale"
    where   bmi = weight / height ^ 2
            skinny = 18.5
            normal = 25.0
            fat = 30.0

f1 :: (Ord a, Num a ) => a -> Bool
f1 x = x <= 0

f2 :: (Fractional a) => a -> a
f2 x = x / 1

