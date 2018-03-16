{-# LANGUAGE ApplicativeDo #-}
module ForkJoin where

import Control.Monad
import Control.Monad.Par.IO
import Data.List
import System.Directory


testForkJoin :: IO ()
testForkJoin = do
  files <- listFiles "/Users/duvalquentin/IdeaProjects/SocialNetwork" ".java"
  mapM_ putStrLn files


listFiles :: FilePath -> String -> IO [FilePath]
listFiles p extension = do
  contents <- map (\subPath -> p ++ "/" ++ subPath)<$> listDirectory p
  -- print (p, contents)
  (dirs, files) <- partitionM doesDirectoryExist contents
  recFiles <- mapM (\subPath -> listFiles subPath extension) dirs
  pure (filter (isSuffixOf extension) files ++ concat recFiles)



partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM p = go [] [] where
  go with without [] = pure (with, without)
  go with without (x:xs) = do
    keep <- p x
    if keep
      then go (x:with) without xs
      else go with (x:without) xs
