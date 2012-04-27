module Traversals where

import Entities
import Control.Exception

import Data.Generics

mapEntityIDs :: (EntityID -> EntityID) -> Entity -> Entity
mapEntityIDs f = everywhere (mkT f)

mapEntityIDsAndModules :: (EntityID -> EntityID)
                       -> (Module -> Module)
                       -> Entity -> Entity
mapEntityIDsAndModules f g = everywhere (mkT f `extT` g)


mapEntityIDsInEntity :: (EntityID -> EntityID) -> Entity -> Entity
mapEntityIDsInEntity f = everywhere (mkT f)

mapTypes :: (HaskellValueType -> HaskellValueType)
         -> (HaskellSelectorType -> HaskellSelectorType)
         -> Entity -> Entity
mapTypes val sel = everywhere (mkT val `extT` sel)

foreachTypeInEntity :: Monad m
                    => (HaskellValueType -> m ())
                    -> (HaskellSelectorType -> m ())
                    -> Entity
                    -> m ()

foreachTypeInEntity val sel e
    = do
        everywhereM (mkM (\x -> sel x >> return x)
                    `extM` (\x -> val x >> return x))
                    e
        return ()

foreachIDInEntity :: Monad m
                  => (EntityID -> m ())
              -> Entity
              -> m ()
foreachIDInEntity f e
    = do
        everywhereM (mkM (\x -> f x >> return x))
                    e
        return ()

mentionedEntityIDs :: Entity -> [EntityID]
mentionedEntityIDs = listify (const True)

deepEvaluateEntity :: Entity -> IO ()
deepEvaluateEntity x
    = everywhereM evaluate x >> return ()
