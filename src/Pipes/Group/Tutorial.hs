{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-| @pipes-group@ builds upon @pipes@ to establish idioms for grouping streams
    into sub-streams without collecting elements into memory.  This tutorial
    assumes familiarity with @pipes@ and @pipes-parse@.
-}

module Pipes.Group.Tutorial (
    -- * Motivation
    -- $motivation

    -- * FreeT
    -- $freeT

    -- * Conclusion
    -- $conclusion
    ) where

import Pipes
import Pipes.Group

{- $motivation
    Dividing a stream into sub-streams is non-trivial.  To illustrate the
    problem, consider the following task: limit a stream to the first three
    groups of elements (a group means consecutive equal elements), without
    loading more than one element into memory.

    The really wrong way to do it is to read each group into memory like this:

> import Lens.Family.State.Strict (zoom)
> import Pipes
> import Pipes.Parse
> import qualified Pipes.Prelude as P
> 
> threeGroups :: (Monad m, Eq a) => Producer a m () -> Producer a m ()
> threeGroups p0 = loop 3 p0
>   where
>     loop 0 _ = return ()
>     loop n p = do
>         (as, p') <- lift $ runStateT (zoom group drawAll) p
>         each as
>         loop (n - 1) p'

    The first problem is that this approach does not output any elements from
    each group until after parsing the entire group:

>>> runEffect $ threeGroups P.stdinLn >-> P.stdoutLn
1<Enter>
1<Enter>
2<Enter>
1
1
2<Enter>
2<Enter>
3<Enter>
2
2
2
4<Enter>
3
>>>

    Worse, this program will crash without outputting a single value if fed an
    infinitely long group of identical elements:

>>> runEffect $ threeGroups (each [1, 1..]) >-> P.print
<Consumes all memory and crashes>

    A better approach is to just stream directly from the first three groups
    instead of storing the groups in intermediate lists:

> import Lens.Family ((^.))
> import Pipes
> import Pipes.Parse
> import qualified Pipes.Prelude as P
> 
> threeGroups :: (Monad m, Eq a) => Producer a m () -> Producer a m ()
> threeGroups p0 = loop 3 p0
>   where
>     loop 0 _ = return ()
>     loop n p = do
>         p' <- p ^. group
>         loop (n - 1) p'

    This will run in constant memory and stream values immediately:

>>> runEffect $ threeGroups P.stdinLn >-> P.stdoutLn
1<Enter>
1
1<Enter>
1
2<Enter>
2
2<Enter>
2
2<Enter>
2
3<Enter>
3
4<Enter>

    However, this code is not very modular: we have to integrate our group
    creation logic with our group consumption logic.  This conflicts with the
    @pipes@ philosophy of decoupling streaming programs into modular components.

    An even better approach would be to split our logic into three steps:

    * Split our 'Producer' into groups

    * Take the first three groups

    * Concatenate these three groups back into a 'Producer'

    But how do we split our `Producer` into groups without loading an entire
    group into memory?  We want to avoid solutions like the following code:

> import Control.Monad (when, liftM2)
> import Lens.Family.State.Strict (zoom)
> import Pipes.Parse
> 
> split :: (Monad m, Eq a) => Producer a m () -> Producer [a] m ()
> split p = do
>     ((as, eof), p') <- lift (runStateT parser p)
>     yield as
>     when (not eof) (split p')
>   where
>     parser = liftM2 (,) (zoom group drawAll) isEndOfInput

    ... because then we're back where we started, loading entire groups into
    memory.
-}

{- $freeT
    Fortunately, you can group elements while still streaming individual
    elements at a time.  The 'FreeT' type @free@ package solves this problem by
    allowing us to build \"linked lists\" of 'Producer's.  This lets you work
    with streams in a list-like manner.

    The key idea is that:

> -- '~' means "is analogous to"
>
> -- If a Producer is like a list
> Producer a m ()            ~   [a]
>
> -- ... then a 'FreeT'-delimited 'Producer' is like a doubly-nested list
> FreeT (Producer a m) m ()  ~  [[a]]

    Think of @FreeT (Producer a m) m ()@ as a \"list of 'Producer's\".  'FreeT'
    nests each subsequent 'Producer' within the return value of the previous
    'Producer' so that you cannot access the next 'Producer' until you
    completely drain the current 'Producer'.  However, you rarely need to work
    with 'FreeT' directly.  Instead, you can structure most things using
    \"splitters\", \"transformations\" and \"joiners\":

> -- A "splitter"
> Producer a m ()           -> FreeT (Producer a m) m ()  ~   [a]  -> [[a]]
>
> -- A "transformation"
> FreeT (Producer a m) m () -> FreeT (Producer a m) m ()  ~  [[a]] -> [[a]]
>
> -- A "joiner"
> FreeT (Producer a m) m () -> Producer a m ()            ~  [[a]] ->  [a]

    An example splitter is @(view groups)@, which splits a 'Producer' into
    'FreeT'-delimited 'Producer's, one for each group of consecutive equal
    elements:

> view groups :: (Eq a, Monad m) => Producer a m x -> FreeT (Producer a m) m x

    An example transformation is @(takes 3)@, which takes the first three
    'Producer's from a 'FreeT' and drops the rest:

> takes 3 :: Monad m => FreeT (Producer a m) m () -> FreeT (Producer a m) m ()

    An example joiner is 'concats', which collapses a 'FreeT' of 'Producer's
    back down into a single 'Producer':

> concats :: Monad m => FreeT (Producer a m) m x -> Producer a m x

    If you compose these three functions together, you will create a function
    that transforms a 'Producer' to keep only the first three groups of
    consecutive equal elements:

> import Lens.Family
> import Pipes
> import Pipes.Group
> import qualified Pipes.Prelude as P
>
> threeGroups :: (Monad m, Eq a) => Producer a m () -> Producer a m ()
> threeGroups = concats . takes 3 . view groups

    Both splitting and joining preserve the streaming nature of 'Producer's and
    do not collect or buffer any values.  The transformed 'Producer' still
    outputs values immediately and does not wait for groups to complete before
    producing results.

>>> runEffect $ threeGroups P.stdinLn >-> P.stdoutLn
1<Enter>
1
1<Enter>
1
2<Enter>
2
3<Enter>
3
3<Enter>
3
4<Enter>
>>>

    Also, lenses simplify things even further.  The reason that 'groups' is a
    lens is because it actually combines both a splitter and joiner into a
    single package.  We can then use 'over' to handle both the splitting and
    joining for us:

>>> runEffect $ over groups (takes 3) P.stdinLn >-> P.stdoutLn

    This gives the exact same behavior because 'over' takes care of calling the
    splitter before applying the transformation, then calling the joiner
    afterward.
-}

{- $conclusion
    This library is very small since it only contains element-agnostic grouping
    utilities.  Downstream libraries that provide richer grouping utilities
    include @pipes-bytestring@ and @pipes-text@.

    If you want to learn how to define your own 'FreeT' splitters, joiners, or
    lenses, you can study existing functions such as 'groupsBy' to learn how to
    use 'FreeT' to introduce boundaries in a stream of 'Producer's.

    To learn more about @pipes-group@, ask questions, or follow development, you
    can subscribe to the @haskell-pipes@ mailing list at:

    <https://groups.google.com/forum/#!forum/haskell-pipes>

    ... or you can mail the list directly at:

    <mailto:haskell-pipes@googlegroups.com>
-}
