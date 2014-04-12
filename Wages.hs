import qualified Cookbook.Project.Quill.Quill      as Qu

import qualified Cookbook.Essential.IO             as Io
import qualified Cookbook.Essential.Common         as Cm
import qualified Cookbook.Essential.Continuous     as Ct

import qualified Cookbook.Ingredients.Lists.Modify as Md

import qualified Cookbook.Recipes.Cline            as Cl
import qualified Cookbook.Recipes.Sanitize         as Sn

import System.IO
import System.Environment
import System.Exit


-- Static

-- | The configuration file that is read for most of Wages' options. It is a Quill database.
confFile = Io.inhome ".wage" ReadMode

-- | The version number for the program (I don't see it getting much past 1.0.0)
version = "1.0.0"

-- | All arguments in the program. This is used for when a user types a faulty command.
allArgs = [("--add",      "Add a wage to the table of wages"),
           ("--list",     "List the previous wages you've recieved"),
           ("--evaluate", "Evaluate a wage without adding it to the file"),
           ("--evalAdd",  "Evaluate a wage and add it to the file"),
           ("--version",  "Print the version of the program"),
           ("--github",   "Print the github for the project"),
           ("--info",     "Print manfile information about the program.")]
          


main = do
  arguments <- getArgs
  case arguments of
    [] -> do
      wRepl
      exitSuccess
    _ -> return ()
  result <- handleArguments (Cl.parse (Cm.flt (Cm.iSp (map Cl.clArg arguments) " ")))
  wF <- wagesFile
  writeFile wF (unlines (map Qu.tableToString (result)))

-- | Handles the arguments by dispatching functions conditionally, and writes to the wages file.
handleArguments :: [Cl.Cline] -> IO ([(String,[(String,String)])])
handleArguments [] = wagesTable
handleArguments (x:xs) = do
  result <- case x of
    (Cl.Argument a b) -> dispatch a b
    (Cl.Flag c) -> dMode c
  handleArguments xs
  wF <- wagesFile
  writeFile wF (unlines (map Qu.tableToString (result)))
  wagesTable

-- | Conditionally dispatches functions over the arguments presented at the command line.
dispatch :: String -> String -> IO ([(String,[(String,String)])])

dispatch "add" a = do
  wT <- wagesTable
  return $ Qu.addItem wT "wages" (".",a)

dispatch "evaluate" a = do
  cTable <- allTables
  pctTitles <- ctLook "splitNames"
  pctNums   <- ctLook "splitPercents"
  let strs = map (\(c,d) -> (c ++ ": " ++ (show ((read a :: Double) * d)))) $ zip (Md.splitOn pctTitles ',') (map (\c -> (read c :: Double)) (Md.splitOn pctNums ',')) -- This line binds the percentage titles and percentages, and even makes them Doubles and pretty-prints them.
  mapM_ putStrLn strs
  wagesTable

dispatch "evalAdd" a = do
  dispatch "evaluate" a
  dispatch "add" a

dispatch a b = do
  dMode 'z'
  wagesTable

-- | Handles single-argument command-line options.
dMode :: Char -> IO ([(String,[(String,String)])])

dMode 'n' = do
  putStrLn version
  wagesTable

dMode 'o' = do
  putStrLn info
  wagesTable
  
dMode 'b' = do
  putStrLn github
  wagesTable

dMode 't' = do
  tb <- wagesTable
  let wagesList = Qu.lookUp tb ("wages",".")
  mapM_ putStrLn $ countPrint (length wagesList) wagesList
  wagesTable
  
dMode a = do
  putStrLn "Incorrect arguments detected! Wages currently supports:"
  mapM_ prettyArguments allArgs
  wagesTable

-- Configuration

-- | Returns all of the tables in the configuration file.
allTables = do
  cFile <- confFile
  allLines <-  fmap lines (hGetContents cFile)
  return $ Qu.tables allLines

-- | Finds the Filename of the wages file.
wagesFile = do
  cTable <- allTables
  return $ head $  (Qu.lookUp cTable ("configuration","wageFile"))

-- | Returns the contents of the wagesFile. Used frequently in functions as a "dummy return".
wagesTable = do
  wFile <- wagesFile 
  allLines <- Io.filelines wFile
  return (Qu.tables allLines)

-- | ConfigurationLook. Looks up information from the configuration file.x
ctLook a = do
  cFile <- allTables
  return $ head $ Qu.lookUp cFile ("configuration",a)

-- Extra

-- | Pretty-print the arguments in a tabular manner.
prettyArguments :: (String,String) -> IO ()
prettyArguments (a,b) = putStrLn (a ++ " : " ++ b)

-- | Display the information about wages.
info = "Files \n ~/.wage is the configuration file for the wages program. It is a Quill database that must have the following items: wageFile, splitNames,splitPercents \n wageFile: The file to save the wages in. \n splitNames: Comma delimited list of \"splits\", which is how you wish to spend your money. \n splitPercents: The percentage of each split, respective to the order in splitNames. \n"

-- | The github for the owner of the project.
github = "http://www.github.com/natepisarski"

-- | Counts off the wages from the lists in the wageFile.
countPrint :: Int -> [String] -> [String]
countPrint 0 _ = []
countPrint x (c:cs) = ("wage " ++ (show x) ++ ": " ++ c) : (countPrint (x - 1) cs)

-- REPL

-- | The message displayed when the REPL is started. Only displayed once.
replMessage = "Welcome to the Wages REPL! This is an interface to wages' command line arguments. Type \"exit\" to exit."

-- | The prompt displayed every iteration of the REPL.
replPrompt = "$ "

-- | The string used to repeat the last command from the REPL.
repeatCommand = "!!"

-- | The Wages REPL starter function. Displays the help message and begins wLoop.
wRepl = do
  putStrLn replMessage
  wLoop ""

-- | The main Wages REPL. Handles special commands (exit and repeat) and builds / executes commands.
wLoop x = do
  query <- Io.prompt replPrompt
  if query == "exit" then exitSuccess else return ()
  if query == repeatCommand then
    do
      action x
    else
    action query
  wLoop query

-- | Makes a change permenantly to the file. This is sometimes just wagesFile.
action query = do
  result <- handleArguments (Cl.parse (("--" ++ (Ct.before query " "))++" "++(Ct.after query " ")))
  wF <- wagesFile
  writeFile wF (unlines (map Qu.tableToString (result)))
