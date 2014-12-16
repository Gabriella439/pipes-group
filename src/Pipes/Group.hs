{-| Element-agnostic grouping utilities for @pipes@

    See "Pipes.Group.Tutorial" for an extended tutorial
-}

{-# LANGUAGE RankNTypes #-}

module Pipes.Group (
    -- * Lenses
    groupsBy,
    groups,
    chunksOf,

    -- * Transformations
    takes,
    takes',
    drops,
    maps,
    individually,

    -- * Joiners
    concats,
    intercalates,

    -- * Folds
    -- $folds
    folds,
    foldsM,

    -- * Re-exports
    -- $reexports
    module Control.Monad.Trans.Class,
    module Control.Monad.Trans.Free,
    module Pipes
    ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Free (FreeF(Pure, Free), FreeT(FreeT, runFreeT))
import qualified Control.Monad.Trans.Free as F
import Data.Functor.Constant (Constant(Constant, getConstant))
import Data.Functor.Identity (Identity(Identity, runIdentity))
import Pipes (Producer, yield, next)
import Pipes.Parse (span, splitAt)
import qualified Pipes as P

import Prelude hiding (span, splitAt)

type Lens a' a b' b = forall f . Functor f => (b' -> f b) -> (a' -> f a)
type Setter a' a b' b = (b' -> Identity b) -> (a' -> Identity a)

(^.) :: a -> ((b -> Constant b b) -> (a -> Constant b a)) -> b
a ^. lens = getConstant (lens Constant a)

{-| 'groupsBy' splits a 'Producer' into a 'FreeT' of 'Producer's grouped using
    the given equality predicate

>>> P.toList . intercalates (P.yield '|') . view (groupsBy (==)) $ P.each "12233345"
"1|22|333|4|5"

    You can think of this as:

> groupsBy
>     :: Monad m
>     => (a -> a -> Bool) -> Lens' (Producer a m x) (FreeT (Producer a m) m x)
-}
groupsBy
    :: Monad m
    => (a' -> a' -> Bool) -> Lens (Producer a' m x) (Producer a m x) (FreeT (Producer a' m) m x) (FreeT (Producer a m) m x)
groupsBy equals k p0 = fmap concats (k (_groupsBy p0))
  where
--  _groupsBy :: Monad m => Producer a m r -> FreeT (Producer a m) m r
    _groupsBy p = FreeT $ do
        x <- next p
        return $ case x of
            Left   r      -> Pure r
            Right (a, p') -> Free $
                fmap _groupsBy ((yield a >> p')^.span (equals a))
{-# INLINABLE groupsBy #-}

{-| Like 'groupsBy', where the equality predicate is ('==')

>>> P.toList . intercalates (P.yield '|') . view groups $ P.each "12233345"
"1|22|333|4|5"

    You can think of this as:

> groups
>     :: (Monad m, Eq a) => Lens' (Producer a m x) (FreeT (Producer a m) m x)
-}
groups :: (Monad m, Eq a') => Lens (Producer a' m x) (Producer a m x) (FreeT (Producer a' m) m x) (FreeT (Producer a m) m x)
groups = groupsBy (==)
{-# INLINABLE groups #-}

{-| 'chunksOf' is an splits a 'Producer' into a 'FreeT' of 'Producer's of fixed
    length

>>> P.toList . intercalates (P.yield '|') . view (chunksOf 3) $ P.each "12233345"
"122|333|45"

    You can think of this as:

> chunksOf
>     :: Monad m => Int -> Lens' (Producer a m x) (FreeT (Producer a m) m x)
-}
chunksOf
    :: Monad m => Int -> Lens (Producer a' m x) (Producer a m x) (FreeT (Producer a' m) m x) (FreeT (Producer a m) m x)
chunksOf n0 k p0 = fmap concats (k (_chunksOf p0))
  where
--  _chunksOf :: Monad m => Producer a m x -> FreeT (Producer a m) m x
    _chunksOf p = FreeT $ do
        x <- next p
        return $ case x of
            Left   r      -> Pure r
            Right (a, p') -> Free $ fmap _chunksOf ((yield a >> p')^.splitAt n0)
{-# INLINABLE chunksOf #-}

-- | Join a 'FreeT'-delimited stream of 'Producer's into a single 'Producer'
concats :: Monad m => FreeT (Producer a m) m x -> Producer a m x
concats = go
  where
    go f = do
        x <- lift (runFreeT f)
        case x of
            Pure r -> return r
            Free p -> do
                f' <- p
                go f'
{-# INLINABLE concats #-}

{-| Join a 'FreeT'-delimited stream of 'Producer's into a single 'Producer' by
    intercalating a 'Producer' in between them
-}
intercalates
    :: Monad m => Producer a m () -> FreeT (Producer a m) m x -> Producer a m x
intercalates sep = go0
  where
    go0 f = do
        x <- lift (runFreeT f)
        case x of
            Pure r -> return r
            Free p -> do
                f' <- p
                go1 f'
    go1 f = do
        x <- lift (runFreeT f)
        case x of
            Pure r -> return r
            Free p -> do
                sep
                f' <- p
                go1 f'
{-# INLINABLE intercalates #-}

{-| @(takes n)@ only keeps the first @n@ functor layers of a 'FreeT'

>>> P.toList . intercalates (P.yield '|') . takes 3 . view groups $ P.each "12233345"
"1|22|333"

    You can think of this as:

> takes
>     :: (Functor f, Monad m)
>     => Int -> FreeT (Producer a m) m () -> FreeT (Producer a m) m ()
-}
takes :: (Functor f, Monad m) => Int -> FreeT f m () -> FreeT f m ()
takes = go
  where
    go n f =
        if (n > 0)
        then FreeT $ do
            x <- runFreeT f
            case x of
                Pure () -> return (Pure ())
                Free w  -> return (Free (fmap (go $! n - 1) w))
        else return ()
{-# INLINABLE takes #-}

{-| @(takes' n)@ only keeps the first @n@ 'Producer's of a 'FreeT'

    'takes'' differs from 'takes' by draining unused 'Producer's in order
    to preserve the return value.  This makes it a suitable argument for 'maps'.
-}
takes' :: Monad m => Int -> FreeT (Producer a m) m x -> FreeT (Producer a m) m x
takes' = go0
  where
    go0 n f = FreeT $
        if (n > 0)
        then do
            x <- runFreeT f
            return $ case x of
                Pure r -> Pure r
                Free p -> Free $ fmap (go0 $! n - 1) p
        else go1 f
    go1 f = do
        x <- runFreeT f
        case x of
            Pure r -> return (Pure r)
            Free p -> do
                f' <- P.runEffect (P.for p P.discard)
                go1 f'
{-# INLINABLE takes' #-}

{-| @(drops n)@ peels off the first @n@ 'Producer' layers of a 'FreeT'

>>> P.toList . intercalates (P.yield '|') . drops 3 . view groups $ P.each "12233345"
"4|5"

    __Use carefully__: the peeling off is not free.   This runs the first @n@
    layers, just discarding everything they produce.
-}
drops :: Monad m => Int -> FreeT (Producer a m) m x -> FreeT (Producer a m) m x
drops = go
  where
    go n ft
        | n <= 0 = ft
        | otherwise = FreeT $ do
            ff <- runFreeT ft
            case ff of
                Pure _ -> return ff
                Free f -> do
                    ft' <- P.runEffect $ P.for f P.discard
                    runFreeT $ go (n-1) ft'
{-# INLINABLE drops #-}

{-| Transform each individual functor layer of a 'FreeT'

    You can think of this as:

> maps
>     :: (forall r . Producer a m r -> Producer b m r)
>     -> FreeT (Producer a m) m x -> FreeT (Producer b m) m x

    This is just a synonym for 'F.transFreeT'
-}
maps
    :: (Monad m, Functor g)
    => (forall r . f r -> g r) -> FreeT f m x -> FreeT g m x
maps = F.transFreeT
{-# INLINABLE maps #-}

{-| Lens to transform each individual functor layer of a 'FreeT'

> over individually = maps  -- ... with a less general type
-}
individually
    :: (Monad m, Functor g)
    => Setter (FreeT f m x) (FreeT g m x) (f (FreeT f m x)) (g (FreeT f m x))
individually nat f0 = Identity (go f0)
  where
    nat' = runIdentity . nat
    go f = FreeT $ do
        x <- runFreeT f
        return $ case x of
            Pure r -> Pure r
            Free w -> Free (fmap go (nat' w))
{-# INLINABLE individually #-}

{- $folds
    These folds are designed to be compatible with the @foldl@ library.  See
    the 'Control.Foldl.purely' and 'Control.Foldl.impurely' functions from that
    library for more details.

    For example, to count the number of 'Producer' layers in a 'FreeT', you can
    write:

> import Control.Applicative (pure)
> import qualified Control.Foldl as L
> import Pipes.Group
> import qualified Pipes.Prelude as P
>
> count :: Monad m => FreeT (Producer a m) m () -> m Int
> count = P.sum . L.purely folds (pure 1)
-}
{-| Fold each 'Producer' of a 'FreeT'

> purely folds
>     :: Monad m => Fold a b -> FreeT (Producer a m) m r -> Producer b m r
-}
folds
    :: Monad m
    => (x -> a -> x)
    -- ^ Step function
    -> x
    -- ^ Initial accumulator
    -> (x -> b)
    -- ^ Extraction function
    -> FreeT (Producer a m) m r
    -- ^
    -> Producer b m r
folds step begin done = go
  where
    go f = do
        x <- lift (runFreeT f)
        case x of
            Pure r -> return r
            Free p -> do
	        (f', b) <- lift (fold p begin)
	        yield b
	        go f'

    fold p x = do
        y <- next p
        case y of
            Left   f      -> return (f, done x)
            Right (a, p') -> fold p' $! step x a
{-# INLINABLE folds #-}

{-| Fold each 'Producer' of a 'FreeT', monadically

> impurely foldsM
>     :: Monad m => FoldM a b -> FreeT (Producer a m) m r -> Producer b m r
-}
foldsM
    :: Monad m
    => (x -> a -> m x)
    -- ^ Step function
    -> m x
    -- ^ Initial accumulator
    -> (x -> m b)
    -- ^ Extraction function
    -> FreeT (Producer a m) m r
    -- ^
    -> Producer b m r
foldsM step begin done = go
  where
    go f = do
        y <- lift (runFreeT f)
        case y of
            Pure r -> return r
            Free p -> do
                (f', b) <- lift $ do
                    x <- begin
		    foldM p x
                yield b
                go f'

    foldM p x = do
        y <- next p
        case y of
            Left   f      -> do
                b <- done x
                return (f, b)
            Right (a, p') -> do
                x' <- step x a
                foldM p' $! x'

{- $reexports
    "Control.Monad.Trans.Class" re-exports 'lift'.

    "Control.Monad.Trans.Free" re-exports 'FreeF' and 'FreeT'

    "Pipes" re-exports 'Producer', 'yield', and 'next'.
-}
