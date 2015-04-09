>{-# LANGUAGE ScopedTypeVariables #-}
> module BaseFunctions where

Misc simple stand-alone functions

#ifndef __HASKELL98__
#define fmap map
#else
#define isAlphanum isAlphaNum
#endif

> import System.IO (stderr, hPutStr)--, isUserError, try)
> import System.IO.Error (isUserError)
> import Control.Exception
> import IOExts_ (unsafePerformIO)
> import System.Environment (getEnv)
> import Data.Maybe(fromMaybe)

> import Data.Char(isAlphanum, isDigit, toLower)
> import Data.List(isPrefixOf, tails)
> import BaseClasses

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Misc

---
`join_words'
 - replace spaces by underscores.

> join_words :: String -> String
> join_words = foldr (\a b -> a ++ '_' : b) "" . words

---
`ioeToString'
  - hack to remove user error tag.
  - not really a good solution !

> ioeToString :: IOError -> String
> ioeToString e
#if __GLASGOW_HASKELL__ == 210
>  = fromMaybe (show e) $ isUserError e
#else 
>  | isUserError e && ue `isPrefixOf` se = drop (length ue) se
>  | otherwise                           = se
>  where
>		ue = show (userError "") 
>		se = show e
#endif

---
`sorted_elem'
  - optimisation for retrieval from sorted list.
  - fail if we are past it.

> sorted_elem :: Ord a => a -> [a] -> Bool
> sorted_elem x [] = False
> sorted_elem x (y:ys)
>  = case compare x y of
>		LT -> False
>		EQ -> True
>		GT -> sorted_elem x ys

---
`isNothing'
  - lacking elsewhere.

#ifndef __HASKELL98__
> isNothing Nothing  = True
> isNothing (Just _) = False
#endif

---
3-tuple accesses

> fst3 (a,_,_) = a
> snd3 (_,a,_) = a
> thd3 (_,_,a) = a
> map_fst3 f (a,b,c) = (f a, b, c)


---
Catch an exception and represent it with Nothing

> exception_to_Nothing :: IO a -> IO (Maybe a)
> exception_to_Nothing io
>  = catch (fmap Just io) (\(e::SomeException) -> return Nothing)

---

> from_env :: String -> IO (Maybe String)
> from_env s = fmap either_to_maybe $ (try::(IO String -> IO (Either SomeException String))) (getEnv s)

---

> either_to_maybe :: Either a b -> Maybe b
> either_to_maybe (Left _)  = Nothing
> either_to_maybe (Right a) = Just a


---
`warn' - print to stderr.
`warn_' - unsafe version of the above (an alternative to traces, can't disable)

> warn :: String -> IO ()
> warn s = hPutStr stderr s

> warn_ :: String -> a -> a
> warn_ s a = unsafePerformIO (warn s >> return a)  
> {-# NoInline warn_ #-}



---
`while' 
  - iteration of a monad whilst its result does not satisfy a predicate.

> while :: Monad m => (a -> m Bool) -> m a -> m a
> while p m 
>  = do
>		x <- m
>		b <- p x 
>		if b
>			then while p m
>			else return x


---
synonym 

> pair = (,) :: a -> b -> (a,b)


---
`sameLength' lazily checks length of 2 lists, ie does not necessarily require
eval of length of both lists.

> sameLength :: [a] -> [a] -> Bool
> sameLength     []     [] = True
> sameLength     []      _ = False
> sameLength      _     [] = False
> sameLength (a:as) (b:bs) = sameLength as bs

---

> unfold h p t = map h . takeWhile p . iterate t
> split n = unfold (take n) (not.null) (drop n)

---
`splitBy' 
  - divides in to sequences of similar, eg adjacent numbers vs adjacent words
  - NB not same as `segmentBy' ...

> splitBy p [] = []
> splitBy p xs 
>  = let (yes,no) = break p xs in 
>    let (yes_p, no_p) = span p no in
>	 if (null yes_p)
>		then yes : splitBy p no_p
>		else yes : yes_p : splitBy p no_p

> splitOn p = filter (all $ not . p) . splitBy p 

---
`segmentBy'
  - eg for splitting arg lists on basis of `head' token starting with "-"

> segmentBy p []     = []
> segmentBy p (x:xs) = let (as,bs) = break p xs in (x : as) : segmentBy p bs

---
Char & String stuff

> isPunct c = not (isAlphanum c || isDigit c || c == '_' || c == '\'')

> lowerise :: String -> String
> lowerise = map toLower

> isSubstringOf :: String -> String -> Bool
> small `isSubstringOf` big = any (isPrefixOf small) $ tails big

---
to switch something off.

> aLWAYS_FAIL :: Bool -> Bool
> aLWAYS_FAIL _ = False

---
`safeRead' 
  - returns a maybe value instead of crashing
  - SURELY there is something similar in the lib/prelude? 
  - based this version on the hugs prelude. 

> safeRead :: Read a => String -> Maybe a
> safeRead s
>  = case [x | (x,t) <- reads s, ("","") <- lex t] of
>		[x] -> Just x
>		_   -> Nothing



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Fns to do with checking of arg lists & arity.

`preciseSplit' splits list, and the first part MUST have n elements.
NB test is on "as", so doesn't check more than n. (consider if xs infinite!)

> preciseSplit :: Int -> [a] -> OkF ([a],[a])
> preciseSplit n xs
>  = let s@(as,_) = splitAt n xs in
>    if (length as < n)
>	 then Fail $ "preciseSplit:need " ++ show n ++ ", got " ++ show (length xs)
>	 else Ok s

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Splitting an Either list.

> splitEither :: [Either b c] -> ([b], [c])
> splitEither [] = ([],[])
> splitEither (x:xs) 
>  = let (bs,cs) = splitEither xs in
>    case x of 
>      Left b  -> (b:bs, cs)
>      Right c -> (bs, c:cs)


