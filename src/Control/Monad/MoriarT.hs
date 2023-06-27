{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Data.Input.Config
Description : My horror at his crimes was lost in my admiration at his skill.
Copyright   : (c) Tom Harding, 2020
License     : MIT

'MoriarT' is a monad transformer implementing the 'MonadCell' class with
provenance and backtracking search. In other words, it can search large
parameter spaces using different parameter configurations, looking for
contradicting sets of parameters to prune out parts of the search tree. It does
this by keeping track of which cells influence which results, and considering
any influencers on a failure to be contradictory.

In other words: if a write to cell @A@ fails, and the write was based on values
from cells @B@ and @C@, any search branch in which @B@ and @C@ have these
current values will be /pruned/ from the search, and we won't try them.

(In practice, this isn't strictly true: we just abort any branch that ever
produces any cell with any provenance that contains those values for @B@ and
@C@. This is a "lazier" strategy, and doesn't involve evaluating the search
space up front).
-}
module Control.Monad.MoriarT
  ( MoriarT (..)

  , runAll
  , runOne
  , solve
  , unsafeRead
  ) where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus, guard)
import Control.Monad.Cell.Class (MonadCell (..))
import qualified Control.Monad.Cell.Class as Cell
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logic (MonadLogic, LogicT (..))
import qualified Control.Monad.Logic as LogicT
import Control.Monad.Primitive (PrimMonad (..))
import Control.Monad.Reader.Class (MonadReader (..))
import qualified Control.Monad.Reader.Class as Reader
import Control.Monad.State.Class (MonadState (..))
import qualified Control.Monad.State.Class as State
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.State (StateT (..))
import qualified Control.Monad.Trans.State as StateT
import qualified Data.CDCL as CDCL
import Data.Foldable (asum)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Input.Config (Config (..))
import Data.JoinSemilattice.Class.Boolean (BooleanR (..))
import Data.JoinSemilattice.Class.Eq (EqR (..))
import Data.JoinSemilattice.Class.Merge (Merge (..), Result (..))
import Data.Kind (Type)
import Data.Maybe (listToMaybe)
import Data.Monoid (Ap (..))
import Data.Primitive.MutVar (MutVar)
import qualified Data.Primitive.MutVar as MutVar
import Data.Propagator (Prop)
import qualified Data.Propagator as Prop
import Type.Reflection (Typeable)

-- | The constraint-solving monad transformer. We implement the current
-- computation context with 'MonadReader', and the current "no goods" list with
-- 'MonadState'.
--
-- This transformer exposes its internals through the 'MonadReader',
-- 'MonadState', 'MonadLogic', and 'MonadIO' interfaces, and should therefore
-- /not/ be used directly. The reason is simply that misuse of any of these
-- will break the computation, so the library provides "Control.Monad.Holmes"
-- and "Control.Monad.Watson", who do their best to thwart 'MoriarT'.
newtype MoriarT (m :: Type -> Type) (x :: Type)
  = MoriarT
      { unMoriarT :: ReaderT CDCL.Rule (LogicT (StateT CDCL.Group m)) x
      }
   deriving newtype
    ( Functor
    , Applicative
    , Alternative
    , Monad
    , MonadIO
    , MonadLogic
    , MonadPlus
    , MonadReader CDCL.Rule
    , MonadState CDCL.Group
    )
  deriving (Semigroup, Monoid)
    via (Ap (MoriarT m) x)

instance MonadTrans MoriarT where
  lift = MoriarT . lift . lift . lift

instance PrimMonad m => PrimMonad (MoriarT m) where
  type PrimState (MoriarT m) = PrimState m

  primitive = lift . primitive

instance PrimMonad m => MonadCell (MoriarT m) where
  newtype Cell (MoriarT m) (content :: Type)
    = Cell (MutVar (PrimState m) (CDCL.Rule, content, MoriarT m ()))

  discard = do
    context <- Reader.ask
    State.modify (CDCL.resolve context) -- Add this context to the "no goods" list.
    
    empty

  fill content = do
    context <- Reader.ask
    mutVar  <- MutVar.newMutVar (context, content, mempty)
    pure (Cell mutVar)

  watch cell@(Cell mutVar) propagator = do
    let next = with cell propagator

    before@(provenance, content, callbacks)
      <- MutVar.readMutVar mutVar

    MutVar.writeMutVar mutVar (provenance, content, callbacks *> next) *> next
      <|> MutVar.writeMutVar mutVar before *> empty -- Undo the action for the next branch.

  with (Cell mutVar) callback = do
    (provenance, content, _) <- MutVar.readMutVar mutVar
    Reader.local (<> provenance) (callback content)

  write (Cell mutVar) news = do
    context <- Reader.ask
    nogoods <- State.get

    before@(provenance, content, callbacks)
      <- MutVar.readMutVar mutVar

    let provenance' = context <> provenance
        content'    = content <<- news

    -- Skip this branch if the provenance is no good.
    guard (not (nogoods `CDCL.implies` provenance'))

    case content' of
      Changed update -> do
        MutVar.writeMutVar mutVar (provenance', update, callbacks) *> callbacks
          <|> MutVar.writeMutVar mutVar before *> empty

      Failure   -> Reader.local (const provenance') discard
      Unchanged -> pure ()

-- | Unsafely read from a cell. This operation is unsafe because it doesn't
-- factor this cell into the provenance of any subsequent writes. If this value
-- ends up causing a contradiction, we may end up removing branches of the
-- search tree that are totally valid! This operation is safe as long as it is
-- the __very last thing__ you do in a computation, and its value is __never__
-- used to influence any writes in any way.
unsafeRead :: PrimMonad m => Cell (MoriarT m) x -> MoriarT m x
unsafeRead (Cell mutVar) = do
  (_, content, _) <- MutVar.readMutVar mutVar

  pure content

-- | Run a 'MoriarT' computation and return the list of __all__ valid branches'
-- results, in the order in which they were discovered.
runAll :: Monad m => MoriarT m x -> m [ x ]
runAll
  = flip StateT.evalStateT mempty
  . LogicT.observeAllT
  . flip runReaderT mempty
  . unMoriarT

-- | Run a 'MoriarT' computation and return the /first/ valid branch's result.
runOne :: Monad m => MoriarT m x -> m (Maybe x)
runOne
  = fmap listToMaybe
  . flip StateT.evalStateT mempty
  . LogicT.observeManyT 1
  . flip runReaderT mempty
  . unMoriarT

-- | Given an input configuration, and a predicate on those input variables,
-- compute the configurations that satisfy the predicate. This result (or these
-- results) can be extracted using 'runOne' or 'runAll'.
solve
  :: ( PrimMonad m
     , EqR f
     , Merge (f x)
     , Typeable x
     )
  => Config (MoriarT m) (f x)
  -> (forall m'. MonadCell m' => [ Prop m' (f x) ] -> Prop m' (f Bool))
  -> MoriarT m [ f x ]
solve Config{..} predicate = do
  inputs <- traverse Cell.fill initial
  output <- Prop.down (predicate (map Prop.up inputs))
  Cell.write output trueR

  _ <- zip [0 ..] inputs & traverse \(major, cell) -> do
    current     <- unsafeRead cell
    refinements <- refine current

    input <- asum $ CDCL.index major refinements <&> \(rule, content) ->
      fmap Cell (MutVar.newMutVar (rule, content, mempty))

    Cell.unify cell input

  traverse unsafeRead inputs
