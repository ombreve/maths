import Data.List (nubBy)

tables :: [[Int]]
tables = map table guests
  where
    table (_,v) = map fst $ filter (orthogonal v . snd) guests
    guests = zip [1..] $ nubBy colinear
        [(x,y,z) | x<-[0..4], y<-[0..4], z<-[0..4], (x,y,z)/=(0,0,0)]
    colinear (x,y,z) (x',y',z') =
        zero (x*y' - x'*y) && zero (x*z' - x'*z) && zero (y*z' - y'*z)
    orthogonal (x,y,z) (x',y',z') = zero (x*x' + y*y' + z*z')
    zero x = x `mod` 5 == 0

main :: IO ()
main = print tables
