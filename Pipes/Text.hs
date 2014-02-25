{-# LANGUAGE RankNTypes, TypeFamilies, BangPatterns, Trustworthy #-}


module Pipes.Text  (
    -- * Introduction
    -- $intro
    
    -- * Producers
    fromLazy

    -- * Pipes
    , map
    , concatMap
    , take
    , drop
    , takeWhile
    , dropWhile
    , filter
    , scan
    , pack
    , unpack
    , toCaseFold
    , toLower
    , toUpper
    , stripStart

    -- * Folds
    , toLazy
    , toLazyM
    , foldChars
    , head
    , last
    , null
    , length
    , any
    , all
    , maximum
    , minimum
    , find
    , index
    , count

    -- * Primitive Character Parsers
    , nextChar
    , drawChar
    , unDrawChar
    , peekChar
    , isEndOfChars

    -- * Parsing Lenses 
    , splitAt
    , span
    , break
    , groupBy
    , group
    , word
    , line

    -- * FreeT Splitters
    , chunksOf
    , splitsWith
    , splits
    , groupsBy
    , groups
    , lines
    , words

    -- * Transformations
    , intersperse
    , packChars
    
    -- * Joiners
    , intercalate
    , unlines
    , unwords

    -- * Re-exports
    -- $reexports
    , module Data.ByteString
    , module Data.Text
    , module Data.Profunctor
    , module Pipes.Parse
    , module Pipes.Group
    ) where

import Control.Applicative ((<*)) 
import Control.Monad (liftM, join)
import Control.Monad.Trans.State.Strict (StateT(..), modify)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Internal (foldrChunks, defaultChunkSize)
import Data.ByteString (ByteString)
import Data.Functor.Constant (Constant(Constant, getConstant))
import Data.Functor.Identity (Identity)
import Data.Profunctor (Profunctor)
import qualified Data.Profunctor
import Pipes
import Pipes.Group (concats, intercalates, FreeT(..), FreeF(..))
import qualified Pipes.Group as PG
import qualified Pipes.Parse as PP
import Pipes.Parse (Parser)
import qualified Pipes.Prelude as P
import Data.Char (isSpace)
import Data.Word (Word8)

import Prelude hiding (
    all,
    any,
    break,
    concat,
    concatMap,
    drop,
    dropWhile,
    elem,
    filter,
    head,
    last,
    lines,
    length,
    map,
    maximum,
    minimum,
    notElem,
    null,
    readFile,
    span,
    splitAt,
    take,
    takeWhile,
    unlines,
    unwords,
    words,
    writeFile )

{- $intro

    * /I. Effectful Text/

    This package provides @pipes@ utilities for /text streams/, understood as
    streams of 'Text' chunks. The individual chunks are uniformly /strict/, and thus you 
    will generally want @Data.Text@ in scope.  But the type @Producer Text m r@ as we
    are using it is a sort of pipes equivalent of the lazy @Text@ type. 
    
    This particular module provides many functions equivalent in one way or another to 
    the pure functions in 
    <https://hackage.haskell.org/package/text-1.1.0.0/docs/Data-Text-Lazy.html Data.Text.Lazy>. 
    They transform, divide, group and fold text streams. Though @Producer Text m r@ 
    is the type of \'effectful Text\', the functions in this module are \'pure\' 
    in the sense that they are uniformly monad-independent.
    Simple /IO/ operations are defined in @Pipes.Text.IO@ -- as lazy IO @Text@ 
    operations are in @Data.Text.Lazy.IO@. Inter-operation with @ByteString@ 
    is provided in @Pipes.Text.Encoding@, which parallels @Data.Text.Lazy.Encoding@. 

    The Text type exported by @Data.Text.Lazy@ is basically that of a lazy list of 
    strict Text: the implementation is arranged so that the individual strict 'Text' 
    chunks are kept to a reasonable size; the user is not aware of the divisions 
    between the connected 'Text' chunks. 
    So also here: the functions in this module are designed to operate on streams that
    are insensitive to text boundaries. This means that they may freely split
    text into smaller texts and /discard empty texts/.  The objective, though, is 
    that they should /never concatenate texts/ in order to provide strict upper 
    bounds on memory usage.  

    For example, to stream only the first three lines of 'stdin' to 'stdout' you
    might write:

> import Pipes
> import qualified Pipes.Text as Text
> import qualified Pipes.Text.IO as Text
> import Pipes.Group (takes')
> import Lens.Family 
> 
> main = runEffect $ takeLines 3 Text.stdin >-> Text.stdout
>   where 
>     takeLines n = Text.unlines . takes' n . view Text.lines

    The above program will never bring more than one chunk of text (~ 32 KB) into
    memory, no matter how long the lines are.

    * /II. Lenses/

    As this example shows, one superficial difference from @Data.Text.Lazy@ 
    is that many of the operations, like 'lines', are \'lensified\'; this has a 
    number of advantages (where it is possible); in particular it facilitates their 
    use with 'Parser's of Text (in the general <http://hackage.haskell.org/package/pipes-parse-3.0.1/docs/Pipes-Parse-Tutorial.html pipes-parse> 
    sense.) The disadvantage, famously, is that the messages you get for type errors can be
    a little alarming. The remarks that follow in this section are for non-lens adepts.

    Each lens exported here, e.g. 'lines', 'chunksOf' or 'splitAt', reduces to the 
    intuitively corresponding function when used with @view@ or @(^.)@. Instead of
    writing:
    
    > splitAt 17 producer
    
    as we would with the Prelude or Text functions, we write 
    
    > view (splitAt 17) producer
    
    or 
    
    > producer ^. splitAt 17

    This may seem a little indirect, but note that many equivalents of 
    @Text -> Text@ functions are exported here as 'Pipe's. Here too we recover the intuitively 
    corresponding functions by prefixing them with @(>->)@. Thus something like

>  stripLines = Text.unlines . Group.maps (>-> Text.stripStart) . view Text.lines 

    would drop the leading white space from each line. 

    The lenses in this library are marked as /improper/; this just means that 
    they don't admit all the operations of an ideal lens, but only "getting" and "focussing". 
    Just for this reason, though, the magnificent complexities of the lens libraries 
    are a distraction. The lens combinators to keep in mind, the ones that make sense for 
    our lenses, are @view@ \/ @(^.)@), @over@ \/ @(%~)@ , and @zoom@. 

    One need only keep in mind that if @l@ is a @Lens' a b@, then:
    
    - @view l@ is a function @a -> b@ . Thus @view l a@ (also written @a ^. l@ ) 
    is the corresponding @b@; as was said above, this function will be exactly the 
    function you think it is, given its name. Thus to uppercase the first n characters 
    of a Producer, leaving the rest the same, we could write: 


    > upper n p = do p' <- p ^. Text.splitAt n >-> Text.toUpper
    >                p'


    - @over l@ is a function @(b -> b) -> a -> a@.  Thus, given a function that modifies
    @b@s, the lens lets us modify an @a@ by applying @f :: b -> b@ to 
    the @b@ that we can \"see\" through the lens. So  @over l f :: a -> a@ 
    (it can also be written @l %~ f@). 
    For any particular @a@, then, @over l f a@ or @(l %~ f) a@ is a revised @a@. 
    So above we might have written things like these: 

    > stripLines = Text.lines %~ maps (>-> Text.stripStart)
    > stripLines = over Text.lines (maps (>-> Text.stripStart))
    > upper n    =  Text.splitAt n %~ (>-> Text.toUpper)
      
    - @zoom l@, finally, is a function from a @Parser b m r@  
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
>     obey p = do (ts, p') <- lift $ runStateT (zoom (Text.splitAt 8) drawAll) p
>                 let seven = T.concat ts
>                 case T.toUpper seven of 
>                    "TOUPPER" -> p' >-> Text.toUpper
>                    "TOLOWER" -> p' >-> Text.toLower
>                    _         -> do yield seven
>                                    p'

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


    * /III.  Special types:/ @Producer Text m (Producer Text m r)@ /and/ @FreeT (Producer Text m) m r@
    
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

    The return type of 'lines', 'words', 'chunksOf' and the other "splitter" functions,
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
    provides many functions for working with things of type @FreeT (Producer a m) m r@
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

-- | Convert a lazy 'TL.Text' into a 'Producer' of strict 'Text's
fromLazy :: (Monad m) => TL.Text -> Producer' Text m ()
fromLazy  = foldrChunks (\e a -> yield e >> a) (return ()) 
{-# INLINE fromLazy #-}


type Lens' a b = forall f . Functor f => (b -> f b) -> (a -> f a)

type Iso' a b = forall f p . (Functor f, Profunctor p) => p b (f b) -> p a (f a)

(^.) :: a -> ((b -> Constant b b) -> (a -> Constant b a)) -> b
a ^. lens = getConstant (lens Constant a)


-- | Apply a transformation to each 'Char' in the stream
map :: (Monad m) => (Char -> Char) -> Pipe Text Text m r
map f = P.map (T.map f)
{-# INLINABLE map #-}

{-# RULES "p >-> map f" forall p f .
        p >-> map f = for p (\txt -> yield (T.map f txt))
  #-}

-- | Map a function over the characters of a text stream and concatenate the results
concatMap
    :: (Monad m) => (Char -> Text) -> Pipe Text Text m r
concatMap f = P.map (T.concatMap f)
{-# INLINABLE concatMap #-}

{-# RULES "p >-> concatMap f" forall p f .
        p >-> concatMap f = for p (\txt -> yield (T.concatMap f txt))
  #-}


-- | Transform a Pipe of 'String's into one of 'Text' chunks
pack :: Monad m => Pipe String Text m r
pack = P.map T.pack
{-# INLINEABLE pack #-}

{-# RULES "p >-> pack" forall p .
        p >-> pack = for p (\txt -> yield (T.pack txt))
  #-}

-- | Transform a Pipes of 'Text' chunks into one of 'String's
unpack :: Monad m => Pipe Text String m r
unpack = for cat (\t -> yield (T.unpack t))
{-# INLINEABLE unpack #-}

{-# RULES "p >-> unpack" forall p .
        p >-> unpack = for p (\txt -> yield (T.unpack txt))
  #-}

-- | @toCaseFold@, @toLower@, @toUpper@ and @stripStart@ are standard 'Text' utilities, 
-- here acting as 'Text' pipes, rather as they would  on a lazy text
toCaseFold :: Monad m => Pipe Text Text m r
toCaseFold = P.map T.toCaseFold
{-# INLINEABLE toCaseFold #-}

{-# RULES "p >-> toCaseFold" forall p .
        p >-> toCaseFold = for p (\txt -> yield (T.toCaseFold txt))
  #-}


-- | lowercase incoming 'Text'
toLower :: Monad m => Pipe Text Text m r
toLower = P.map T.toLower
{-# INLINEABLE toLower #-}

{-# RULES "p >-> toLower" forall p .
        p >-> toLower = for p (\txt -> yield (T.toLower txt))
  #-}

-- | uppercase incoming 'Text'
toUpper :: Monad m => Pipe Text Text m r
toUpper = P.map T.toUpper
{-# INLINEABLE toUpper #-}

{-# RULES "p >-> toUpper" forall p .
        p >-> toUpper = for p (\txt -> yield (T.toUpper txt))
  #-}

-- | Remove leading white space from an incoming succession of 'Text's 
stripStart :: Monad m => Pipe Text Text m r
stripStart = do
    chunk <- await
    let text = T.stripStart chunk
    if T.null text
      then stripStart
      else do yield text 
              cat
{-# INLINEABLE stripStart #-}

-- | @(take n)@ only allows @n@ individual characters to pass; 
--  contrast @Pipes.Prelude.take@ which would let @n@ chunks pass.
take :: (Monad m, Integral a) => a -> Pipe Text Text m ()
take n0 = go n0 where
    go n
        | n <= 0    = return ()
        | otherwise = do
            txt <- await
            let len = fromIntegral (T.length txt)
            if (len > n)
                then yield (T.take (fromIntegral n) txt)
                else do
                    yield txt
                    go (n - len)
{-# INLINABLE take #-}

-- | @(drop n)@ drops the first @n@ characters
drop :: (Monad m, Integral a) => a -> Pipe Text Text m r
drop n0 = go n0 where
    go n
        | n <= 0    = cat
        | otherwise = do
            txt <- await
            let len = fromIntegral (T.length txt)
            if (len >= n)
                then do
                    yield (T.drop (fromIntegral n) txt)
                    cat
                else go (n - len)
{-# INLINABLE drop #-}

-- | Take characters until they fail the predicate
takeWhile :: (Monad m) => (Char -> Bool) -> Pipe Text Text m ()
takeWhile predicate = go
  where
    go = do
        txt <- await
        let (prefix, suffix) = T.span predicate txt
        if (T.null suffix)
            then do
                yield txt
                go
            else yield prefix
{-# INLINABLE takeWhile #-}

-- | Drop characters until they fail the predicate
dropWhile :: (Monad m) => (Char -> Bool) -> Pipe Text Text m r
dropWhile predicate = go where
    go = do
        txt <- await
        case T.findIndex (not . predicate) txt of
            Nothing -> go
            Just i -> do
                yield (T.drop i txt)
                cat
{-# INLINABLE dropWhile #-}

-- | Only allows 'Char's to pass if they satisfy the predicate
filter :: (Monad m) => (Char -> Bool) -> Pipe Text Text m r
filter predicate = P.map (T.filter predicate)
{-# INLINABLE filter #-}

{-# RULES "p >-> filter q" forall p q .
        p >-> filter q = for p (\txt -> yield (T.filter q txt))
  #-}
  
-- | Strict left scan over the characters
scan
    :: (Monad m)
    => (Char -> Char -> Char) -> Char -> Pipe Text Text m r
scan step begin = do
    yield (T.singleton begin)
    go begin
  where
    go c = do
        txt <- await
        let txt' = T.scanl step c txt
            c' = T.last txt'
        yield (T.tail txt')
        go c'
{-# INLINABLE scan #-}

{-| Fold a pure 'Producer' of strict 'Text's into a lazy
    'TL.Text'
-}
toLazy :: Producer Text Identity () -> TL.Text
toLazy = TL.fromChunks . P.toList
{-# INLINABLE toLazy #-}

{-| Fold an effectful 'Producer' of strict 'Text's into a lazy
    'TL.Text'

    Note: 'toLazyM' is not an idiomatic use of @pipes@, but I provide it for
    simple testing purposes.  Idiomatic @pipes@ style consumes the chunks
    immediately as they are generated instead of loading them all into memory.
-}
toLazyM :: (Monad m) => Producer Text m () -> m TL.Text
toLazyM = liftM TL.fromChunks . P.toListM
{-# INLINABLE toLazyM #-}

-- | Reduce the text stream using a strict left fold over characters
foldChars
    :: Monad m
    => (x -> Char -> x) -> x -> (x -> r) -> Producer Text m () -> m r
foldChars step begin done = P.fold (T.foldl' step) begin done
{-# INLINABLE foldChars #-}

-- | Retrieve the first 'Char'
head :: (Monad m) => Producer Text m () -> m (Maybe Char)
head = go
  where
    go p = do
        x <- nextChar p
        case x of
            Left   _      -> return  Nothing
            Right (c, _) -> return (Just c)
{-# INLINABLE head #-}

-- | Retrieve the last 'Char'
last :: (Monad m) => Producer Text m () -> m (Maybe Char)
last = go Nothing
  where
    go r p = do
        x <- next p
        case x of
            Left   ()      -> return r
            Right (txt, p') ->
                if (T.null txt)
                then go r p'
                else go (Just $ T.last txt) p'
{-# INLINABLE last #-}

-- | Determine if the stream is empty
null :: (Monad m) => Producer Text m () -> m Bool
null = P.all T.null
{-# INLINABLE null #-}

-- | Count the number of characters in the stream
length :: (Monad m, Num n) => Producer Text m () -> m n
length = P.fold (\n txt -> n + fromIntegral (T.length txt)) 0 id
{-# INLINABLE length #-}

-- | Fold that returns whether 'M.Any' received 'Char's satisfy the predicate
any :: (Monad m) => (Char -> Bool) -> Producer Text m () -> m Bool
any predicate = P.any (T.any predicate)
{-# INLINABLE any #-}

-- | Fold that returns whether 'M.All' received 'Char's satisfy the predicate
all :: (Monad m) => (Char -> Bool) -> Producer Text m () -> m Bool
all predicate = P.all (T.all predicate)
{-# INLINABLE all #-}

-- | Return the maximum 'Char' within a text stream
maximum :: (Monad m) => Producer Text m () -> m (Maybe Char)
maximum = P.fold step Nothing id
  where
    step mc txt =
        if (T.null txt)
        then mc
        else Just $ case mc of
            Nothing -> T.maximum txt
            Just c -> max c (T.maximum txt)
{-# INLINABLE maximum #-}

-- | Return the minimum 'Char' within a text stream (surely very useful!)
minimum :: (Monad m) => Producer Text m () -> m (Maybe Char)
minimum = P.fold step Nothing id
  where
    step mc txt =
        if (T.null txt)
        then mc
        else case mc of
            Nothing -> Just (T.minimum txt)
            Just c -> Just (min c (T.minimum txt))
{-# INLINABLE minimum #-}

-- | Find the first element in the stream that matches the predicate
find
    :: (Monad m)
    => (Char -> Bool) -> Producer Text m () -> m (Maybe Char)
find predicate p = head (p >-> filter predicate)
{-# INLINABLE find #-}

-- | Index into a text stream
index
    :: (Monad m, Integral a)
    => a-> Producer Text m () -> m (Maybe Char)
index n p = head (p >-> drop n)
{-# INLINABLE index #-}


-- | Store a tally of how many segments match the given 'Text'
count :: (Monad m, Num n) => Text -> Producer Text m () -> m n
count c p = P.fold (+) 0 id (p >-> P.map (fromIntegral . T.count c))
{-# INLINABLE count #-}


-- | Consume the first character from a stream of 'Text'
-- 
-- 'next' either fails with a 'Left' if the 'Producer' has no more characters or
-- succeeds with a 'Right' providing the next character and the remainder of the
-- 'Producer'.

nextChar
    :: (Monad m)
    => Producer Text m r
    -> m (Either r (Char, Producer Text m r))
nextChar = go
  where
    go p = do
        x <- next p
        case x of
            Left   r       -> return (Left r)
            Right (txt, p') -> case (T.uncons txt) of
                Nothing        -> go p'
                Just (c, txt') -> return (Right (c, yield txt' >> p'))
{-# INLINABLE nextChar #-}

-- | Draw one 'Char' from a stream of 'Text', returning 'Left' if the 'Producer' is empty

drawChar :: (Monad m) => Parser Text m (Maybe Char)
drawChar = do
    x <- PP.draw
    case x of
        Nothing  -> return Nothing
        Just txt -> case (T.uncons txt) of
            Nothing        -> drawChar
            Just (c, txt') -> do
                PP.unDraw txt'
                return (Just c)
{-# INLINABLE drawChar #-}

-- | Push back a 'Char' onto the underlying 'Producer'
unDrawChar :: (Monad m) => Char -> Parser Text m ()
unDrawChar c = modify (yield (T.singleton c) >>)
{-# INLINABLE unDrawChar #-}

{-| 'peekChar' checks the first 'Char' in the stream, but uses 'unDrawChar' to
    push the 'Char' back

> peekChar = do
>     x <- drawChar
>     case x of
>         Left  _  -> return ()
>         Right c -> unDrawChar c
>     return x

-}

peekChar :: (Monad m) => Parser Text m (Maybe Char)
peekChar = do
    x <- drawChar
    case x of
        Nothing  -> return ()
        Just c -> unDrawChar c
    return x
{-# INLINABLE peekChar #-}

{-| Check if the underlying 'Producer' has no more characters

    Note that this will skip over empty 'Text' chunks, unlike
    'PP.isEndOfInput' from @pipes-parse@, which would consider
    an empty 'Text' a valid bit of input.

> isEndOfChars = liftM isLeft peekChar
-}
isEndOfChars :: (Monad m) => Parser Text m Bool
isEndOfChars = do
    x <- peekChar
    return (case x of
        Nothing -> True
        Just _-> False )
{-# INLINABLE isEndOfChars #-}


-- | Splits a 'Producer' after the given number of characters
splitAt
    :: (Monad m, Integral n)
    => n
    -> Lens' (Producer Text m r)
             (Producer Text m (Producer Text m r))
splitAt n0 k p0 = fmap join (k (go n0 p0))
  where
    go 0 p = return p
    go n p = do
        x <- lift (next p)
        case x of
            Left   r       -> return (return r)
            Right (txt, p') -> do
                let len = fromIntegral (T.length txt)
                if (len <= n)
                    then do
                        yield txt
                        go (n - len) p'
                    else do
                        let (prefix, suffix) = T.splitAt (fromIntegral n) txt
                        yield prefix
                        return (yield suffix >> p')
{-# INLINABLE splitAt #-}


-- | Split a text stream in two, producing the longest
--   consecutive group of characters that satisfies the predicate
--   and returning the rest

span
    :: (Monad m)
    => (Char -> Bool)
    -> Lens' (Producer Text m r)
             (Producer Text m (Producer Text m r))
span predicate k p0 = fmap join (k (go p0))
  where
    go p = do
        x <- lift (next p)
        case x of
            Left   r       -> return (return r)
            Right (txt, p') -> do
                let (prefix, suffix) = T.span predicate txt
                if (T.null suffix)
                    then do
                        yield txt
                        go p'
                    else do
                        yield prefix
                        return (yield suffix >> p')
{-# INLINABLE span #-}

{-| Split a text stream in two, producing the longest
    consecutive group of characters that don't satisfy the predicate
-}
break
    :: (Monad m)
    => (Char -> Bool)
    -> Lens' (Producer Text m r)
             (Producer Text m (Producer Text m r))
break predicate = span (not . predicate)
{-# INLINABLE break #-}

{-| Improper lens that splits after the first group of equivalent Chars, as
    defined by the given equivalence relation
-}
groupBy
    :: (Monad m)
    => (Char -> Char -> Bool)
    -> Lens' (Producer Text m r)
             (Producer Text m (Producer Text m r))
groupBy equals k p0 = fmap join (k ((go p0))) where
    go p = do
        x <- lift (next p)
        case x of
            Left   r       -> return (return r)
            Right (txt, p') -> case T.uncons txt of
                Nothing      -> go p'
                Just (c, _) -> (yield txt >> p') ^. span (equals c) 
{-# INLINABLE groupBy #-}

-- | Improper lens that splits after the first succession of identical 'Char' s
group :: Monad m 
      => Lens' (Producer Text m r)
               (Producer Text m (Producer Text m r))
group = groupBy (==)
{-# INLINABLE group #-}

{-| Improper lens that splits a 'Producer' after the first word

    Unlike 'words', this does not drop leading whitespace 
-}
word :: (Monad m) 
     => Lens' (Producer Text m r)
              (Producer Text m (Producer Text m r))
word k p0 = fmap join (k (to p0))
  where
    to p = do
        p' <- p^.span isSpace
        p'^.break isSpace
{-# INLINABLE word #-}


line :: (Monad m) 
     => Lens' (Producer Text m r)
              (Producer Text m (Producer Text m r))
line = break (== '\n')

{-# INLINABLE line #-}


-- | Intersperse a 'Char' in between the characters of stream of 'Text'
intersperse
    :: (Monad m) => Char -> Producer Text m r -> Producer Text m r
intersperse c = go0
  where
    go0 p = do
        x <- lift (next p)
        case x of
            Left   r       -> return r
            Right (txt, p') -> do
                yield (T.intersperse c txt)
                go1 p'
    go1 p = do
        x <- lift (next p)
        case x of
            Left   r       -> return r
            Right (txt, p') -> do
                yield (T.singleton c)
                yield (T.intersperse c txt)
                go1 p'
{-# INLINABLE intersperse #-}



-- | Improper isomorphism between a 'Producer' of 'ByteString's and 'Word8's
packChars :: Monad m => Iso' (Producer Char m x) (Producer Text m x)
packChars = Data.Profunctor.dimap to (fmap from)
  where
    -- to :: Monad m => Producer Char m x -> Producer Text m x
    to p = PG.folds step id done (p^.PG.chunksOf defaultChunkSize)

    step diffAs c = diffAs . (c:)

    done diffAs = T.pack (diffAs [])

    -- from :: Monad m => Producer Text m x -> Producer Char m x
    from p = for p (each . T.unpack)
{-# INLINABLE packChars #-}


-- | Split a text stream into 'FreeT'-delimited text streams of fixed size
chunksOf
    :: (Monad m, Integral n)
    => n -> Lens' (Producer Text m r) 
                  (FreeT (Producer Text m) m r)
chunksOf n k p0 = fmap concats (k (FreeT (go p0)))
  where
    go p = do
        x <- next p
        return $ case x of
            Left   r       -> Pure r
            Right (txt, p') -> Free $ do
                p'' <- (yield txt >> p') ^. splitAt n 
                return $ FreeT (go p'')
{-# INLINABLE chunksOf #-}


{-| Split a text stream into sub-streams delimited by characters that satisfy the
    predicate
-}
splitsWith
    :: (Monad m)
    => (Char -> Bool)
    -> Producer Text m r
    -> FreeT (Producer Text m) m r
splitsWith predicate p0 = FreeT (go0 p0)
  where
    go0 p = do
        x <- next p
        case x of
            Left   r       -> return (Pure r)
            Right (txt, p') ->
                if (T.null txt)
                then go0 p'
                else return $ Free $ do
                    p'' <-  (yield txt >> p') ^. span (not . predicate)
                    return $ FreeT (go1 p'')
    go1 p = do
        x <- nextChar p
        return $ case x of
            Left   r      -> Pure r
            Right (_, p') -> Free $ do
                    p'' <- p' ^. span (not . predicate) 
                    return $ FreeT (go1 p'')
{-# INLINABLE splitsWith #-}

-- | Split a text stream using the given 'Char' as the delimiter
splits :: (Monad m)
      => Char
      -> Lens' (Producer Text m r)
               (FreeT (Producer Text m) m r)
splits c k p =
          fmap (PG.intercalates (yield (T.singleton c))) (k (splitsWith (c ==) p))
{-# INLINABLE splits #-}

{-| Isomorphism between a stream of 'Text' and groups of equivalent 'Char's , using the
    given equivalence relation
-}
groupsBy
    :: Monad m
    => (Char -> Char -> Bool)
    -> Lens' (Producer Text m x) (FreeT (Producer Text m) m x)
groupsBy equals k p0 = fmap concats (k (FreeT (go p0))) where 
  go p = do x <- next p
            case x of Left   r       -> return (Pure r)
                      Right (bs, p') -> case T.uncons bs of
                             Nothing      -> go p'
                             Just (c, _) -> do return $ Free $ do
                                                 p'' <- (yield bs >> p')^.span (equals c)
                                                 return $ FreeT (go p'')
{-# INLINABLE groupsBy #-}


-- | Like 'groupsBy', where the equality predicate is ('==')
groups
    :: Monad m
    => Lens' (Producer Text m x) (FreeT (Producer Text m) m x)
groups = groupsBy (==)
{-# INLINABLE groups #-}



{-| Split a text stream into 'FreeT'-delimited lines
-}
lines
    :: (Monad m) => Iso' (Producer Text m r)  (FreeT (Producer Text m) m r)
lines = Data.Profunctor.dimap _lines (fmap _unlines)
  where
  _lines p0 = FreeT (go0 p0) 
    where
      go0 p = do
              x <- next p
              case x of
                  Left   r       -> return (Pure r)
                  Right (txt, p') ->
                      if (T.null txt)
                      then go0 p'
                      else return $ Free $ go1 (yield txt >> p')
      go1 p = do
              p' <- p ^. break ('\n' ==)
              return $ FreeT $ do
                  x  <- nextChar p'
                  case x of
                      Left   r      -> return $ Pure r
                      Right (_, p'') -> go0 p''
  -- _unlines
  --     :: Monad m
  --      => FreeT (Producer Text m) m x -> Producer Text m x
  _unlines = concats . PG.maps (<* yield (T.singleton '\n'))
  

{-# INLINABLE lines #-}


-- | Split a text stream into 'FreeT'-delimited words
words
    :: (Monad m) => Iso' (Producer Text m r) (FreeT (Producer Text m) m r)
words = Data.Profunctor.dimap go (fmap _unwords)
  where
    go p = FreeT $ do
        x <- next (p >-> dropWhile isSpace)
        return $ case x of
            Left   r       -> Pure r
            Right (bs, p') -> Free $ do
                p'' <-  (yield bs >> p') ^. break isSpace
                return (go p'')
    _unwords = PG.intercalates (yield $ T.singleton ' ')
    
{-# INLINABLE words #-}


{-| 'intercalate' concatenates the 'FreeT'-delimited text streams after
    interspersing a text stream in between them
-}
intercalate
    :: (Monad m)
    => Producer Text m ()
    -> FreeT (Producer Text m) m r
    -> Producer Text m r
intercalate p0 = go0
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
                p0
                f' <- p
                go1 f'
{-# INLINABLE intercalate #-}

{-| Join 'FreeT'-delimited lines into a text stream
-}
unlines
    :: (Monad m) => FreeT (Producer Text m) m r -> Producer Text m r
unlines = go
  where
    go f = do
        x <- lift (runFreeT f)
        case x of
            Pure r -> return r
            Free p -> do
                f' <- p
                yield $ T.singleton '\n'
                go f'
{-# INLINABLE unlines #-}

{-| Join 'FreeT'-delimited words into a text stream
-}
unwords
    :: (Monad m) => FreeT (Producer Text m) m r -> Producer Text m r
unwords = intercalate (yield $ T.singleton ' ')
{-# INLINABLE unwords #-}


{- $reexports
    
    @Data.Text@ re-exports the 'Text' type.

    @Pipes.Parse@ re-exports 'input', 'concat', 'FreeT' (the type) and the 'Parse' synonym. 
-}


