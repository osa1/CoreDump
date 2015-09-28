module CoreDump (plugin) where

import GhcPlugins

import Data.IORef (writeIORef)
import Data.List (intersperse)

-- For pretty-printing
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Pretty

import CoreDump.Show

plugin :: Plugin
plugin = defaultPlugin{installCoreToDos = installPlugin}
  where
    installPlugin :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
    installPlugin opts todos = do
      getDynFlags >>= liftIO . writeIORef dynFlags_ref
      -- trace ("opts: " ++ show opts) (return ())
      return $ concat
        [ if before then [CoreDoPluginPass "CoreDump - before" pluginPass] else []
        , todos
        , if after then [CoreDoPluginPass "CoreDump - after" pluginPass] else []
        ]
      where
        both   = "both"   `elem` opts
        before = "before" `elem` opts || both
        after  = "after"  `elem` opts || both

pluginPass :: ModGuts -> CoreM ModGuts
pluginPass guts@ModGuts{ mg_binds = binds } = do
    liftIO $ mapM_ putStrLn $ intersperse "" $ map showPretty binds
    return guts

showPretty :: Show a => a -> String
showPretty a =
    case parseExp s of
      ParseOk x          -> prettyPrint x
      -- ParseFailed loc reason ->
      --   trace ("parse failed: " ++ show (loc, reason)) s
      ParseFailed{}      -> s
  where
    s = show a
