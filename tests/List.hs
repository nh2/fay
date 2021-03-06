import           FFI
import           Prelude hiding (take)

main = putStrLn (showList (take 5 (let ns = 1 : map' (\x -> x + 1) ns in ns)))

take 0 _      = []
take n (x:xs) = x : take (n - 1) xs

map' f []     = []
map' f (x:xs) = f x : map' f xs

showList :: [Int] -> String
showList = ffi "JSON.stringify(%1)"
