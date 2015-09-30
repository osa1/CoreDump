module CoreDump (plugin) where

import GhcPlugins

import Data.IORef (writeIORef)
import Data.List (intersperse)

-- For pretty-printing
import Text.Show.Pretty (ppDoc)
import Text.PrettyPrint

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
    liftIO $ mapM_ putStrLn $ intersperse "" $
      map (renderStyle (Style PageMode 100 1.0) . ppDoc) binds
    return guts
