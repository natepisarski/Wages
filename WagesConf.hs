module WagesConf where
import qualified Cookbook.Essential.IO as IO
import qualified Cookbook.Project.Quill.Quill2.Meta as Qm

import System.IO

fl_syn = ".wage"
wf_syn = "wagesFile"
cf_syn = "configuration"
em_syn = "Error! Missing "
ed_syn = "Error! Multiple "

allArguments = [("add x", "Add x to the wages file."),
                ("eval x", "Evaluate the earnings of x based on splits."),
                ("evalAdd x", "Evaluate the earnings, and then add x to the file.")]
               
-- | Get the configuration file handle.
confFile = IO.inhome fl_syn ReadMode

-- | Return the configuration database.
confDB   = do
  hdl <- confFile
  ln  <- fmap lines $ hGetContents hdl
  return $ Qm.pFile ln

-- | Handle Quill errors with error messages, or return the Quill.
qError :: Qm.QuillStatus a -> a
qError x = case x of
  (Qm.QuillSuccess a)  -> a
  (Qm.QuillMultiple a) -> error (ed_syn  ++ a)
  (Qm.QuillMissing a)  -> error (em_syn  ++ a)
  
-- | Get the filename for the wages file.
wfName = do
  cdb <- confDB
  return $ Qm.lookUp cdb (cf_syn, wf_syn)

-- | Return the lines of the WagesFile as a database.
wfDB = do
  wName <- wfName
  dB    <- Qm.fromFile (qError wName)
  return dB

eExtractList :: Qm.Element String -> [String]
eExtractList (Qm.List c) = c

eExtractTable :: Qm.Element String -> [(String,String)]
eExtractTable (Qm.Table c) = c

getSplits = do
  wDB <- wfDB
  
  let sTable = eExtractTable $ snd (qError (Qm.getQuill wDB "splits"))
  return [(c, (read d :: Double)) | (c,d) <- sTable]
