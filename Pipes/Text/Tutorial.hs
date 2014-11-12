{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Pipes.Text.Tutorial (
    -- * Effectful Text
    -- $intro
    -- ** @Pipes.Text@
    -- $pipestext
    -- ** @Pipes.Text.IO@
    -- $pipestextio
    -- ** @Pipes.Text.Encoding@
    -- $pipestextencoding
    -- * Lenses
    -- $lenses

    -- ** @view@ \/ @(^.)@
    -- $view

    -- ** @over@ \/ @(%~)@
    -- $over

    -- ** @zoom@
    -- $zoom

    -- * Special types: @Producer Text m (Producer Text m r)@ and @FreeT (Producer Text m) m r@
    -- $special
    ) where

import Pipes
import Pipes.Text
import Pipes.Text.IO
import Pipes.Text.Encoding

{- $intro
    This package provides @pipes@ utilities for /character streams/,
    realized as streams of 'Text' chunks. The individual chunks are uniformly /strict/,
    and thus the @Text@ type we are using is the one from @Data.Text@, not @Data.Text.Lazy@ 
    But the type @Producer Text m r@, as we are using it, is a sort of /pipes/ equivalent of 
    the lazy @Text@ type.

    The main @Pipes.Text@ module provides many functions equivalent 
    in one way or another to the pure functions in
    <https://hackage.haskell.org/package/text-1.1.0.0/docs/Data-Text-Lazy.html Data.Text.Lazy> 
    (and the corresponding @Prelude@ functions for @String@ s): they transform, 
    divide, group and fold text streams. Though @Producer Text m r@
    is the type of \'effectful Text\', the functions in @Pipes.Text@ are \'pure\'
    in the sense that they are uniformly monad-independent.
    Simple /IO/ operations are defined in @Pipes.Text.IO@ - as lazy IO @Text@
    operations are in @Data.Text.Lazy.IO@. Similarly, as @Data.Text.Lazy.Encoding@ 
    handles inter-operation with @Data.ByteString.Lazy@, @Pipes.Text.Encoding@ provides for
    interoperation with the \'effectful ByteStrings\' of @Pipes.ByteString@.

    Remember that the @Text@ type exported by @Data.Text.Lazy@ is basically 
    that of a lazy list of strict @Text@: the implementation is arranged so that 
    the individual strict 'Text' chunks are kept to a reasonable size; the user 
    is not aware of the divisions between the connected 'Text' chunks, but uses
    operations akin to those for strict text.
    So also here: the functions in this module are designed to operate on character streams that
    in a way that is independent of the boundaries of the underlying @Text@ chunks. 
    This means that they may freely split text into smaller texts and /discard empty texts/.  
    The objective, though, is that they should not /concatenate texts/ in order to provide strict upper
    bounds on memory usage.

    For example, to stream only the first three lines of 'stdin' to 'stdout' you
    might write:

> import Pipes
> import qualified Pipes.Text as Text
> import qualified Pipes.Text.IO as Text
> import Pipes.Group (takes')
> import Lens.Family (view)
>
> main = runEffect $ takeLines 3 Text.stdin >-> Text.stdout
>   where
>     takeLines n = view Text.unlines . takes' n . view Text.lines

     This program will never bring more into memory than what @Text.stdin@ considers
     one chunk of text (~ 32 KB), even if individual lines are split across many chunks.

-}
{- $lenses
    As the use of @view@ in this example shows, one superficial difference from @Data.Text.Lazy@
    is that many of the operations, like 'lines', are \'lensified\'; this has a
    number of advantages; in particular it facilitates their use with 'Parser's of Text 
    (in the general <http://hackage.haskell.org/package/pipes-parse-3.0.1/docs/Pipes-Parse-Tutorial.html pipes-parse>
    sense.) The remarks that follow in this section are for non-lens adepts.

    Each lens exported here, e.g. 'lines', 'chunksOf' or 'splitAt', reduces to the
    intuitively corresponding function when used with @view@ or @(^.)@. Instead of
    writing:

    > splitAt 17 producer

    as we would with the Prelude or Text functions, we write

    > view (splitAt 17) producer

    or equivalently

    > producer ^. splitAt 17

    This may seem a little indirect, but note that many equivalents of
    @Text -> Text@ functions are exported here as 'Pipe's. Here too we recover the intuitively
    corresponding functions by prefixing them with @(>->)@. Thus something like

>  stripLines =  view Text.unlines . Group.maps (>-> Text.stripStart) . view Text.lines

    would drop the leading white space from each line. 

    The lenses in this library are marked as /improper/; this just means that
    they don't admit all the operations of an ideal lens, but only /getting/ and /focusing/.
    Just for this reason, though, the magnificent complexities of the lens libraries
    are a distraction. The lens combinators to keep in mind, the ones that make sense for
    our lenses, are @view@ \/ @(^.)@), @over@ \/ @(%~)@ , and @zoom@.

    One need only keep in mind that if @l@ is a @Lens' a b@, then:

-}
{- $view
    @view l@ is a function @a -> b@ . Thus @view l a@ (also written @a ^. l@ )
    is the corresponding @b@; as was said above, this function will typically be 
    the pipes equivalent of the function you think it is, given its name. So for example 
    
    > view (Text.drop)
    > view (Text.splitAt 300) :: Producer Text m r -> Producer Text (Producer Text m r)
    > Text.stdin ^. splitAt 300 :: Producer Text IO (Producer Text IO r) 
    
    I.e., it produces the first 300 characters, and returns the rest of the producer. 
    Thus to uppercase the first n characters
    of a Producer, leaving the rest the same, we could write:


    > upper n p = do p' <- p ^. Text.splitAt n >-> Text.toUpper
    >                p'
-}
{- $over
    @over l@ is a function @(b -> b) -> a -> a@.  Thus, given a function that modifies
    @b@s, the lens lets us modify an @a@ by applying @f :: b -> b@ to
    the @b@ that we can \"see\" through the lens. So  @over l f :: a -> a@
    (it can also be written @l %~ f@).
    For any particular @a@, then, @over l f a@ or @(l %~ f) a@ is a revised @a@.
    So above we might have written things like these:

    > stripLines = Text.lines %~ maps (>-> Text.stripStart)
    > stripLines = over Text.lines (maps (>-> Text.stripStart))
    > upper n    =  Text.splitAt n %~ (>-> Text.toUpper)

-}
{- $zoom
    @zoom l@, finally, is a function from a @Parser b m r@
    to a @Parser a m r@ (or more generally a @StateT (Producer b m x) m r@).
    Its use is easiest to see with an decoding lens like 'utf8', which
    \"sees\" a Text producer hidden inside a ByteString producer:
    @drawChar@ is a Text parser, returning a @Maybe Char@, @zoom utf8 drawChar@ is
    a /ByteString/ parser, returning a @Maybe Char@. @drawAll@ is a Parser that returns
    a list of everything produced from a Producer, leaving only the return value; it would
    usually be unreasonable to use it. But @zoom (splitAt 17) drawAll@
    returns a list of Text chunks containing the first seventeen Chars, and returns the rest of
    the Text Producer for further parsing. Suppose that we want, inexplicably, to
    modify the casing of a Text Producer according to any instruction it might
    contain at the start. Then we might write something like this:

>     obey :: Monad m => Producer Text m b -> Producer Text m b
>     obey p = do (ts, p') <- lift $ runStateT (zoom (Text.splitAt 7) drawAll) p
>                 let seven = T.concat ts
>                 case T.toUpper seven of
>                    "TOUPPER" -> p' >-> Text.toUpper
>                    "TOLOWER" -> p' >-> Text.toLower
>                    _         -> do yield seven
>                                    p'


> >>> let doc = each ["toU","pperTh","is document.\n"]
> >>> runEffect $ obey doc >-> Text.stdout
> THIS DOCUMENT.

    The purpose of exporting lenses is the mental economy achieved with this three-way
    applicability. That one expression, e.g. @lines@ or @splitAt 17@ can have these
    three uses is no more surprising than that a pipe can act as a function modifying
    the output of a producer, namely by using @>->@ to its left: @producer >-> pipe@
    -- but can /also/ modify the inputs to a consumer by using @>->@ to its right:
    @pipe >-> consumer@

    The three functions, @view@ \/ @(^.)@, @over@ \/ @(%~)@ and @zoom@ are supplied by
    both <http://hackage.haskell.org/package/lens lens> and
    <http://hackage.haskell.org/package/lens-family lens-family> The use of 'zoom' is explained
    in <http://hackage.haskell.org/package/pipes-parse-3.0.1/docs/Pipes-Parse-Tutorial.html Pipes.Parse.Tutorial>
    and to some extent in the @Pipes.Text.Encoding@ module here.

-}
{- $special
    These simple 'lines' examples reveal a more important difference from @Data.Text.Lazy@ .
    This is in the types that are most closely associated with our central text type,
    @Producer Text m r@.  In @Data.Text@ and @Data.Text.Lazy@ we find functions like

>   splitAt  :: Int -> Text -> (Text, Text)
>   lines    ::        Text -> [Text]
>   chunksOf :: Int -> Text -> [Text]

    which relate a Text with a pair of Texts or a list of Texts.
    The corresponding functions here (taking account of \'lensification\') are

>   view . splitAt  :: (Monad m, Integral n) => n -> Producer Text m r -> Producer Text m (Producer Text m r)
>   view lines      :: Monad m               =>      Producer Text m r -> FreeT (Producer Text m) m r
>   view . chunksOf :: (Monad m, Integral n) => n -> Producer Text m r -> FreeT (Producer Text m) m r

    Some of the types may be more readable if you imagine that we have introduced
    our own type synonyms

>   type Text m r  = Producer T.Text m r
>   type Texts m r = FreeT (Producer T.Text m) m r

    Then we would think of the types above as

>   view . splitAt  :: (Monad m, Integral n) => n -> Text m r -> Text m (Text m r)
>   view lines      :: (Monad m)             =>      Text m r -> Texts m r
>   view . chunksOf :: (Monad m, Integral n) => n -> Text m r -> Texts m r

    which brings one closer to the types of the similar functions in @Data.Text.Lazy@

    In the type @Producer Text m (Producer Text m r)@ the second
    element of the \'pair\' of effectful Texts cannot simply be retrieved
    with something like 'snd'. This is an \'effectful\' pair, and one must work
    through the effects of the first element to arrive at the second Text stream, even
    if you are proposing to throw the Text in the first element away.
    Note that we use Control.Monad.join to fuse the pair back together, since it specializes to

>    join :: Monad m => Producer Text m (Producer m r) -> Producer m r

    The return type of 'lines', 'words', 'chunksOf' and the other /splitter/ functions,
    @FreeT (Producer m Text) m r@ -- our @Texts m r@ -- is the type of (effectful)
    lists of (effectful) texts. The type @([Text],r)@ might be seen to gather
    together things of the forms:

> r
> (Text,r)
> (Text, (Text, r))
> (Text, (Text, (Text, r)))
> (Text, (Text, (Text, (Text, r))))
> ...

    (We might also have identified the sum of those types with @Free ((,) Text) r@
    -- or, more absurdly, @FreeT ((,) Text) Identity r@.)

    Similarly, our type @Texts m r@, or @FreeT (Text m) m r@ -- in fact called
    @FreeT (Producer Text m) m r@ here -- encompasses all the members of the sequence:

> m r
> Text m r
> Text m (Text m r)
> Text m (Text m (Text m r))
> Text m (Text m (Text m (Text m r)))
> ...

    We might have used a more specialized type in place of @FreeT (Producer a m) m r@,
    or indeed of @FreeT (Producer Text m) m r@, but it is clear that the correct
    result type of 'lines' will be isomorphic to @FreeT (Producer Text m) m r@ .

    One might think that

>   lines :: Monad m => Lens' (Producer Text m r) (FreeT (Producer Text m) m r)
>   view . lines :: Monad m => Producer Text m r -> FreeT (Producer Text m) m r

    should really have the type

>   lines :: Monad m => Pipe Text Text m r

    as e.g. 'toUpper' does. But this would spoil the control we are
    attempting to maintain over the size of chunks. It is in fact just
    as unreasonable to want such a pipe as to want

> Data.Text.Lazy.lines :: Text -> Text

    to 'rechunk' the strict Text chunks inside the lazy Text to respect
    line boundaries. In fact we have

> Data.Text.Lazy.lines :: Text -> [Text]
> Prelude.lines :: String -> [String]

    where the elements of the list are themselves lazy Texts or Strings; the use
    of @FreeT (Producer Text m) m r@ is simply the 'effectful' version of this.

    The @Pipes.Group@ module, which can generally be imported without qualification,
    provides many functions for working with things of type @FreeT (Producer a m) m r@.
    In particular it conveniently exports the constructors for @FreeT@ and the associated
    @FreeF@ type -- a fancy form of @Either@, namely

> data FreeF f a b = Pure a | Free (f b)

    for pattern-matching. Consider the implementation of the 'words' function, or
    of the part of the lens that takes us to the words; it is compact but exhibits many
    of the points under discussion, including explicit handling of the @FreeT@ and @FreeF@
    constuctors.  Keep in mind that

>  newtype FreeT f m a  = FreeT (m (FreeF f a (FreeT f m a)))
>  next :: Monad m => Producer a m r -> m (Either r (a, Producer a m r))

   Thus the @do@ block after the @FreeT@ constructor is in the base monad, e.g. 'IO' or 'Identity';
   the later subordinate block, opened by the @Free@ constructor, is in the @Producer@ monad:

> words :: Monad m => Producer Text m r -> FreeT (Producer Text m) m r
> words p = FreeT $ do                   -- With 'next' we will inspect p's first chunk, excluding spaces;
>   x <- next (p >-> dropWhile isSpace)  --   note that 'dropWhile isSpace' is a pipe, and is thus *applied* with '>->'.
>   return $ case x of                   -- We use 'return' and so need something of type 'FreeF (Text m) r (Texts m r)'
>     Left   r       -> Pure r           -- 'Left' means we got no Text chunk, but only the return value; so we are done.
>     Right (txt, p') -> Free $ do       -- If we get a chunk and the rest of the producer, p', we enter the 'Producer' monad
>         p'' <- view (break isSpace)    -- When we apply 'break isSpace', we get a Producer that returns a Producer;
>                     (yield txt >> p')  --   so here we yield everything up to the next space, and get the rest back.
>         return (words p'')             -- We then carry on with the rest, which is likely to begin with space.

-}
