import qualified Cookbook.Project.Quill.Quill2.Meta as Qm

import qualified Cookbook.Essential.Meta as Cm
import qualified Cookbook.Ingredients.Lists.Modify as Md


import System.IO
import System.Environment
import System.Exit

import WagesConf
main = do
  arguments <- getArgs
  if null arguments then wRepl else dispatch arguments

dispatch :: [String] -> IO ()

dispatch ("add":x) = do
  wf <- wfDB
  fN <- fmap qError (wfName)
  Qm.toFile fN (qError (Qm.addItem wf (Qm.AList ("wages",(head x)))))
  putStrLn "Added"

dispatch ("eval":x) = do
  wF <- wfDB
  fN <- fmap qError (wfName)
  splitUp <- getSplits
  mapM_ (pPrint (read (head x) :: Double)) splitUp
  where
    pPrint a (b,c) = do
      putStrLn $ b ++ " : " ++ (show (a * c))

dispatch ("evalAdd":x) = do
  dispatch ("eval":x)
  dispatch ("add":x)

dispatch [] = return ()
dispatch (x:xs) = dispatch xs

wRepl = do
  cmd <- Cm.prompt "$ "
  dispatch (Md.splitOn cmd ' ')
