-- sum.hs

{-# LANGUAGE PatternGuards #-}
import Prelude hiding ( catch )
import Control.Exception
import Control.Monad ( liftM )
import System.IO

main = style1

style1 = interact ((++ "\n") . show . sum . map read . lines)

style2 = catch (interact ((++ "\n") . show . sum . map read . lines))
         (\e -> hPutStrLn stderr ("couldn't sum lines: " ++ show e))

style3 = do
  m <- hGetContents stdin
  nums <- mapM readM . lines $ m
  print (sum nums)
  `catch` (\e -> hPutStrLn stderr ("couldn't sum lines: " ++ show e))


readM :: (Monad m, Read a) => String -> m a
readM s | [x] <- parse = return x
        | otherwise    = fail $ "Failed to parse \"" ++ s ++ "\" as a number."
  where
    parse = [x | (x,_) <- reads s]
