{-# LANGUAGE CPP #-}

module GHC.Driver.Env
   ( Hsc(..)
   , HscEnv (..)
   , hsc_home_unit
   , hsc_units
   , runHsc
   , mkInteractiveHscEnv
   , runInteractiveHsc
   , hscEPS
   , hptCompleteSigs
   , hptInstances
   , hptAnns
   , hptAllThings
   , hptSomeThingsBelowUs
   , hptRules
   , prepareAnnotations
   , lookupType
   , lookupIfaceByModule
   , mainModIs
   )
where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Driver.Ppr
import GHC.Driver.Session
import GHC.Driver.Errors ( printOrThrowWarnings )

import GHC.Runtime.Context
import GHC.Driver.Env.Types ( Hsc(..), HscEnv(..) )

import GHC.Unit
import GHC.Unit.Module.ModGuts
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.ModDetails
import GHC.Unit.Module.Deps
import GHC.Unit.Home.ModInfo
import GHC.Unit.Env
import GHC.Unit.External

import GHC.Core         ( CoreRule )
import GHC.Core.FamInstEnv
import GHC.Core.InstEnv ( ClsInst )

import GHC.Types.Annotations ( Annotation, AnnEnv, mkAnnEnv, plusAnnEnv )
import GHC.Types.CompleteMatch
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.TyThing

import GHC.Builtin.Names ( gHC_PRIM )

import GHC.Data.Maybe
import GHC.Data.Bag

import GHC.Utils.Outputable
import GHC.Utils.Monad
import GHC.Utils.Panic
import GHC.Utils.Misc

import Control.Monad    ( guard )
import Data.IORef

runHsc :: HscEnv -> Hsc a -> IO a
runHsc hsc_env (Hsc hsc) = do
    (a, w) <- hsc hsc_env emptyBag
    printOrThrowWarnings (hsc_logger hsc_env) (hsc_dflags hsc_env) w
    return a

-- | Switches in the DynFlags and Plugins from the InteractiveContext
mkInteractiveHscEnv :: HscEnv -> HscEnv
mkInteractiveHscEnv hsc_env =
    let ic = hsc_IC hsc_env
    in hsc_env { hsc_dflags  = ic_dflags  ic
               , hsc_plugins = ic_plugins ic
               }

-- | A variant of runHsc that switches in the DynFlags and Plugins from the
-- InteractiveContext before running the Hsc computation.
runInteractiveHsc :: HscEnv -> Hsc a -> IO a
runInteractiveHsc hsc_env = runHsc (mkInteractiveHscEnv hsc_env)

hsc_home_unit :: HscEnv -> HomeUnit
hsc_home_unit = ue_home_unit . hsc_unit_env

hsc_units :: HscEnv -> UnitState
hsc_units = ue_units . hsc_unit_env

{-

Note [Target code interpreter]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Template Haskell and GHCi use an interpreter to execute code that is built for
the compiler target platform (= code host platform) on the compiler host
platform (= code build platform).

The internal interpreter can be used when both platforms are the same and when
the built code is compatible with the compiler itself (same way, etc.). This
interpreter is not always available: for instance stage1 compiler doesn't have
it because there might be an ABI mismatch between the code objects (built by
stage1 compiler) and the stage1 compiler itself (built by stage0 compiler).

In most cases, an external interpreter can be used instead: it runs in a
separate process and it communicates with the compiler via a two-way message
passing channel. The process is lazily spawned to avoid overhead when it is not
used.

The target code interpreter to use can be selected per session via the
`hsc_interp` field of `HscEnv`. There may be no interpreter available at all, in
which case Template Haskell and GHCi will fail to run. The interpreter to use is
configured via command-line flags (in `GHC.setSessionDynFlags`).


-}

-- Note [hsc_type_env_var hack]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- hsc_type_env_var is used to initialize tcg_type_env_var, and
-- eventually it is the mutable variable that is queried from
-- if_rec_types to get a TypeEnv.  So, clearly, it's something
-- related to knot-tying (see Note [Tying the knot]).
-- hsc_type_env_var is used in two places: initTcRn (where
-- it initializes tcg_type_env_var) and initIfaceCheck
-- (where it initializes if_rec_types).
--
-- But why do we need a way to feed a mutable variable in?  Why
-- can't we just initialize tcg_type_env_var when we start
-- typechecking?  The problem is we need to knot-tie the
-- EPS, and we may start adding things to the EPS before type
-- checking starts.
--
-- Here is a concrete example. Suppose we are running
-- "ghc -c A.hs", and we have this file system state:
--
--  A.hs-boot   A.hi-boot **up to date**
--  B.hs        B.hi      **up to date**
--  A.hs        A.hi      **stale**
--
-- The first thing we do is run checkOldIface on A.hi.
-- checkOldIface will call loadInterface on B.hi so it can
-- get its hands on the fingerprints, to find out if A.hi
-- needs recompilation.  But loadInterface also populates
-- the EPS!  And so if compilation turns out to be necessary,
-- as it is in this case, the thunks we put into the EPS for
-- B.hi need to have the correct if_rec_types mutable variable
-- to query.
--
-- If the mutable variable is only allocated WHEN we start
-- typechecking, then that's too late: we can't get the
-- information to the thunks.  So we need to pre-commit
-- to a type variable in 'hscIncrementalCompile' BEFORE we
-- check the old interface.
--
-- This is all a massive hack because arguably checkOldIface
-- should not populate the EPS. But that's a refactor for
-- another day.

-- | Retrieve the ExternalPackageState cache.
hscEPS :: HscEnv -> IO ExternalPackageState
hscEPS hsc_env = readIORef (hsc_EPS hsc_env)

hptCompleteSigs :: HscEnv -> [CompleteMatch]
hptCompleteSigs = hptAllThings  (md_complete_matches . hm_details)

-- | Find all the instance declarations (of classes and families) from
-- the Home Package Table filtered by the provided predicate function.
-- Used in @tcRnImports@, to select the instances that are in the
-- transitive closure of imports from the currently compiled module.
hptInstances :: HscEnv -> (ModuleName -> Bool) -> ([ClsInst], [FamInst])
hptInstances hsc_env want_this_module
  = let (insts, famInsts) = unzip $ flip hptAllThings hsc_env $ \mod_info -> do
                guard (want_this_module (moduleName (mi_module (hm_iface mod_info))))
                let details = hm_details mod_info
                return (md_insts details, md_fam_insts details)
    in (concat insts, concat famInsts)

-- | Get rules from modules "below" this one (in the dependency sense)
hptRules :: HscEnv -> [ModuleNameWithIsBoot] -> [CoreRule]
hptRules = hptSomeThingsBelowUs (md_rules . hm_details) False


-- | Get annotations from modules "below" this one (in the dependency sense)
hptAnns :: HscEnv -> Maybe [ModuleNameWithIsBoot] -> [Annotation]
hptAnns hsc_env (Just deps) = hptSomeThingsBelowUs (md_anns . hm_details) False hsc_env deps
hptAnns hsc_env Nothing = hptAllThings (md_anns . hm_details) hsc_env

hptAllThings :: (HomeModInfo -> [a]) -> HscEnv -> [a]
hptAllThings extract hsc_env = concatMap extract (eltsHpt (hsc_HPT hsc_env))

-- | Get things from modules "below" this one (in the dependency sense)
-- C.f Inst.hptInstances
hptSomeThingsBelowUs :: (HomeModInfo -> [a]) -> Bool -> HscEnv -> [ModuleNameWithIsBoot] -> [a]
hptSomeThingsBelowUs extract include_hi_boot hsc_env deps
  | isOneShot (ghcMode (hsc_dflags hsc_env)) = []

  | otherwise
  = let hpt = hsc_HPT hsc_env
    in
    [ thing
    |   -- Find each non-hi-boot module below me
      GWIB { gwib_mod = mod, gwib_isBoot = is_boot } <- deps
    , include_hi_boot || (is_boot == NotBoot)

        -- unsavoury: when compiling the base package with --make, we
        -- sometimes try to look up RULES etc for GHC.Prim. GHC.Prim won't
        -- be in the HPT, because we never compile it; it's in the EPT
        -- instead. ToDo: clean up, and remove this slightly bogus filter:
    , mod /= moduleName gHC_PRIM

        -- Look it up in the HPT
    , let things = case lookupHpt hpt mod of
                    Just info -> extract info
                    Nothing -> pprTrace "WARNING in hptSomeThingsBelowUs" msg []
          msg = vcat [text "missing module" <+> ppr mod,
                      text "Probable cause: out-of-date interface files"]
                        -- This really shouldn't happen, but see #962

        -- And get its dfuns
    , thing <- things ]


-- | Deal with gathering annotations in from all possible places
--   and combining them into a single 'AnnEnv'
prepareAnnotations :: HscEnv -> Maybe ModGuts -> IO AnnEnv
prepareAnnotations hsc_env mb_guts = do
    eps <- hscEPS hsc_env
    let -- Extract annotations from the module being compiled if supplied one
        mb_this_module_anns = fmap (mkAnnEnv . mg_anns) mb_guts
        -- Extract dependencies of the module if we are supplied one,
        -- otherwise load annotations from all home package table
        -- entries regardless of dependency ordering.
        home_pkg_anns  = (mkAnnEnv . hptAnns hsc_env) $ fmap (dep_mods . mg_deps) mb_guts
        other_pkg_anns = eps_ann_env eps
        ann_env        = foldl1' plusAnnEnv $ catMaybes [mb_this_module_anns,
                                                         Just home_pkg_anns,
                                                         Just other_pkg_anns]
    return ann_env

-- | Find the 'TyThing' for the given 'Name' by using all the resources
-- at our disposal: the compiled modules in the 'HomePackageTable' and the
-- compiled modules in other packages that live in 'PackageTypeEnv'. Note
-- that this does NOT look up the 'TyThing' in the module being compiled: you
-- have to do that yourself, if desired
lookupType :: HscEnv -> Name -> IO (Maybe TyThing)
lookupType hsc_env name = do
   eps <- liftIO $ readIORef (hsc_EPS hsc_env)
   let pte = eps_PTE eps
       hpt = hsc_HPT hsc_env

       mod = ASSERT2( isExternalName name, ppr name )
             if isHoleName name
               then mkHomeModule (hsc_home_unit hsc_env) (moduleName (nameModule name))
               else nameModule name

       !ty = if isOneShot (ghcMode (hsc_dflags hsc_env))
               -- in one-shot, we don't use the HPT
               then lookupNameEnv pte name
               else case lookupHptByModule hpt mod of
                Just hm -> lookupNameEnv (md_types (hm_details hm)) name
                Nothing -> lookupNameEnv pte name
   pure ty

-- | Find the 'ModIface' for a 'Module', searching in both the loaded home
-- and external package module information
lookupIfaceByModule
        :: HomePackageTable
        -> PackageIfaceTable
        -> Module
        -> Maybe ModIface
lookupIfaceByModule hpt pit mod
  = case lookupHptByModule hpt mod of
       Just hm -> Just (hm_iface hm)
       Nothing -> lookupModuleEnv pit mod
   -- If the module does come from the home package, why do we look in the PIT as well?
   -- (a) In OneShot mode, even home-package modules accumulate in the PIT
   -- (b) Even in Batch (--make) mode, there is *one* case where a home-package
   --     module is in the PIT, namely GHC.Prim when compiling the base package.
   -- We could eliminate (b) if we wanted, by making GHC.Prim belong to a package
   -- of its own, but it doesn't seem worth the bother.

mainModIs :: HscEnv -> Module
mainModIs hsc_env = mkHomeModule (hsc_home_unit hsc_env) (mainModuleNameIs (hsc_dflags hsc_env))
