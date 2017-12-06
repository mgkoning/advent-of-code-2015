import Data.List
import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.Char8 as C

findHash key prefix = head $ filter (isPrefixOf prefix . snd) hashes
  where
    hashes = [(index, hash) | index <- [1..], let hash = show $ md5 $ C.pack $ key ++ show index]

solution = do
  putStrLn $ show $ findHash "abcdef" "00000"
  putStrLn $ show $ findHash "pqrstuv" "00000" 
  putStrLn $ show $ findHash "yzbqklnj" "00000"
  putStrLn $ show $ findHash "yzbqklnj" "000000"