myDiv1 :: Int -> Int -> Int
myDiv1 x 0 = error "Division by zero"
myDiv1 x y = x `div` y
