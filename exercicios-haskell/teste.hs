import Data.Map (Map)
import qualified Data.Map as Map

fib :: Integer -> Integer
fib n = fibMemo Map.! n
  where
    fibMemo = Map.fromList [(i, fib' i) | i <- [0..n]]
    fib' 0 = 0
    fib' 1 = 1
    fib' i = (fibMemo Map.! (i - 1)) + (fibMemo Map.! (i - 2))

main :: IO ()
main = do
    putStrLn "Digite um número:"
    input <- getLine
    let n = read input :: Integer
    print (fib n)
