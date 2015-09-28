{-# LANGUAGE FlexibleInstances, StandaloneDeriving, TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | We define lots of orphan Show instances here, for debugging and learning
-- purposes.
--
-- Most of the time while trying to figure out when a constructor is used or how
-- is a term compiled, it's easiest to just create an example and run the plugin
-- on it.
--
-- Without Show instances though, we can't easily inspect compiled outputs.
-- Outputable generated strings hide lots of details(especially constructors),
-- but we still export a `showOutputable` here, for similar reasons.
--
module CoreDump.Show where

-- import Control.Monad.IO.Class
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

import Class
import CostCentre
import Demand
import ForeignCall
import GhcPlugins
import IdInfo
import PrimOp
import TypeRep

import Prelude

--------------------------------------------------------------------------------

{-# NOINLINE dynFlags_ref #-}
dynFlags_ref :: IORef DynFlags
dynFlags_ref = unsafePerformIO (newIORef undefined)

{-# NOINLINE dynFlags #-}
dynFlags :: DynFlags
dynFlags = unsafePerformIO (readIORef dynFlags_ref)

-- initDynFlags :: (HasDynFlags m, MonadIO m) => m ()
-- initDynFlags = getDynFlags >>= liftIO .writeIORef dynFlags

showOutputable :: Outputable a => a -> String
showOutputable = showSDoc dynFlags . ppr

--------------------------------------------------------------------------------
-- * Orphan Show instances

deriving instance Show a => Show (Expr a)
deriving instance Show Type
deriving instance Show Literal
deriving instance Show a => Show (Tickish a)
deriving instance Show a => Show (Bind a)
deriving instance Show AltCon
deriving instance Show TyLit
deriving instance Show FunctionOrData
deriving instance Show Module
deriving instance Show CostCentre
deriving instance Show Role
deriving instance Show LeftOrRight
deriving instance Show IsCafCC

instance Show Class where
  show _ = "Class"

deriving instance Show IdDetails
deriving instance Show PrimOp
deriving instance Show ForeignCall
deriving instance Show TickBoxOp
deriving instance Show PrimOpVecCat
deriving instance Show CCallSpec
deriving instance Show CCallTarget
deriving instance Show CCallConv
deriving instance Show SpecInfo
deriving instance Show OccInfo
deriving instance Show InlinePragma
deriving instance Show OneShotInfo
deriving instance Show CafInfo
deriving instance Show Unfolding
deriving instance Show UnfoldingSource
deriving instance Show UnfoldingGuidance
deriving instance Show Activation
deriving instance Show CoreRule
-- deriving instance Show IsOrphan
deriving instance Show StrictSig
deriving instance Show DmdType

instance Show RuleFun where
  show _ = "RuleFun"

instance Show (UniqFM a) where
  show _ = "UniqFM"

data SIdInfo
  = IdInfo Arity SpecInfo Unfolding CafInfo
           OneShotInfo InlinePragInfo OccInfo
           StrictSig Demand ArityInfo
  deriving (Show)

instance Show IdInfo where
  show info = show (IdInfo arityInfo_ specInfo_ unfoldingInfo_
                           cafInfo_ oneShotInfo_ inlinePragInfo_
                           occInfo_ strictnessInfo_ demandInfo_
                           callArityInfo_)
    where
      arityInfo_ = arityInfo info
      specInfo_  = specInfo info
      unfoldingInfo_ = unfoldingInfo info
      cafInfo_   = cafInfo info
      oneShotInfo_ = oneShotInfo info
      inlinePragInfo_ = inlinePragInfo info
      occInfo_ = occInfo info
      strictnessInfo_ = strictnessInfo info
      demandInfo_ = demandInfo info
      callArityInfo_ = callArityInfo info

-- Unique's Show instance is not parseable by haskell-src-exts,
-- disabling it for now
data SId = Id Name {- Unique -} Type IdDetails IdInfo
  deriving (Show)

data STyVar = TyVar Name
  deriving (Show)

instance Show Var where
    show v
      | isId v    = show (Id name {- uniq_ -} ty details info)
      | otherwise = show (TyVar name)
      where
        name = varName v
        -- uniq_ = varUnique v
        ty   = varType v

        details = idDetails v
        info    = idInfo v

instance Show DataCon where
    show = show . dataConName

instance Show TyCon where
    show = show . tyConName

instance Show ModuleName where
    show = show . moduleNameString

instance Show PackageKey where
    show = show . packageKeyString

instance Show Name where
    show = showOutputable . nameOccName

-- deriving instance Show Name
instance Show OccName where
    show = showOutputable

instance Show Coercion where
    show _ = "Coercion"

-- Instance for non-terms related stuff.

deriving instance Show CoreToDo
deriving instance Show SimplifierMode
deriving instance Show CompilerPhase
deriving instance Show FloatOutSwitches

instance Show PluginPass where
    show _ = "PluginPass"
