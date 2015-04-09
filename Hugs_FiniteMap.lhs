
This code is derived from from the paper :
	S Adams
	"Efficient sets: a balancing act"
	Journal of functional programming 3(4) Oct 1993, pp553-562

This module is a copy of the FiniteMap.lhs in the library of ghc-0.21, but 
has more informative comments on the functionality of the module. 

This MUST NOT be imported in any ghc compilation - it is for HUGS only.
(ghc has its own superior library version)

Main comments are given at the end of the imports.

> module Hugs_FiniteMap (
> 	FiniteMap,		-- abstract type
> 
> 	emptyFM, singletonFM, listToFM,
> 
> 	addToFM,   addListToFM,
> 	addToFM_C, addListToFM_C,
> 	delFromFM, delListFromFM,
> 
> 	plusFM,      plusFM_C,
> 	intersectFM, intersectFM_C,
> 	minusFM, 
> 
> 	mapFM, foldFM, filterFM,
> 
> 	sizeFM,	lookupFM, lookupWithDefaultFM,
> 	
> 	fmToList, keysFM, eltsFM,
> 
> 	Maybe
>     ) where

> import Data.Maybe

--------------------------------------------------------------------------------

This ADT implements tables as balanced binary trees. A balanced binary tree 
is a binary tree with the depth as low as possible. This means lookup
complexity will be O(log n) (this is efficient).

Functions include:-
Standard lookup, add, delete and tree information etc...
 
 %************************************************************************
 \subsection{The @FiniteMap@ data type, and building of same}
 %************************************************************************

This defines the ADT used within this module. 
 
> data FiniteMap key elt
>   = EmptyFM 
>   | Branch key elt	    	-- Key and elt stored here
>     Int
>     (FiniteMap key elt)	    	-- Children
>     (FiniteMap key elt)
> 

emptyFM returns an empty table.

> emptyFM	:: FiniteMap key elt
> emptyFM = EmptyFM
> 

singletonFM returns a table with only one key/element.

> singletonFM	:: key -> elt -> FiniteMap key elt
> singletonFM key elt = Branch key elt 1 emptyFM emptyFM
> 

listToFM produces the table from a list of key element pairs.

> listToFM	:: Ord key => [(key,elt)] -> FiniteMap key elt
> listToFM key_elt_pairs = addListToFM emptyFM key_elt_pairs
 
 %************************************************************************
 \subsection{Adding to and deleting from @FiniteMaps@}
 %************************************************************************

addToFM adds a sinlge key element pair to a table.

> addToFM
>  :: Ord key => FiniteMap key elt -> key -> elt  -> FiniteMap key elt
> addToFM fm key elt = addToFM_C (\ old new -> new) fm key elt

 addToFM_C adds a single element to a table but if there is already a
 value there (for the given key) then the function given is performed
on the two values for the key.
 
> addToFM_C	:: Ord key => (elt -> elt -> elt)
> 			   -> FiniteMap key elt -> key -> elt  
> 			   -> FiniteMap key elt
> addToFM_C combiner EmptyFM key elt = singletonFM key elt
> addToFM_C combiner (Branch key elt size fm_l fm_r) new_key new_elt
>   | new_key < key = mkBalBranch key elt
>                          (addToFM_C combiner fm_l new_key new_elt) fm_r
>   | new_key > key = mkBalBranch key elt fm_l
>                          (addToFM_C combiner fm_r new_key new_elt)
>   | otherwise	  = Branch new_key (combiner elt new_elt) size fm_l fm_r

addListToFM adds a list of key element pairs to a table.

> addListToFM
>  :: Ord key => FiniteMap key elt -> [(key,elt)] -> FiniteMap key elt
> addListToFM fm key_elt_pairs = addListToFM_C (\ old new -> new)
>                                     fm key_elt_pairs

addListToFM_C adds a list of key element pairs to a table but if there is
already a value there (for the given key) then the function given is performed
on the two values for the key.

> addListToFM_C	:: Ord key => (elt -> elt -> elt)
> 			   -> FiniteMap key elt -> [(key,elt)] 
> 			   -> FiniteMap key elt
> addListToFM_C combiner fm key_elt_pairs
>   = foldl add fm key_elt_pairs	-- foldl adds from the left
>   where
>     add fm (key,elt) = addToFM_C combiner fm key elt

delFromFM deletes the value for the given key from the given table.

> delFromFM	:: Ord key => FiniteMap key elt -> key   -> FiniteMap key elt
> delFromFM EmptyFM del_key = emptyFM
> delFromFM (Branch key elt size fm_l fm_r) del_key
>   | del_key > key
>   = mkBalBranch key elt fm_l (delFromFM fm_r del_key)
> 
>   | del_key < key
>   = mkBalBranch key elt (delFromFM fm_l del_key) fm_r
> 
>   | key == del_key
>   = glueBal fm_l fm_r

delListFromFM deletes the values for the given list of keys from the table.

> delListFromFM	:: Ord key => FiniteMap key elt -> [key] -> FiniteMap key elt
> delListFromFM fm keys = foldl delFromFM fm keys
 
 %************************************************************************
 \subsection{Combining @FiniteMaps@}
 %************************************************************************

plusFM_C combines two tables into a single table.  If a key is present in
both tables the function given is performed on the two values for the key.
 
> plusFM_C	:: Ord key => (elt -> elt -> elt) 
> 			   -> FiniteMap key elt
>                          -> FiniteMap key elt
>                          -> FiniteMap key elt
> plusFM_C combiner EmptyFM fm2 = fm2
> plusFM_C combiner fm1 EmptyFM = fm1
> plusFM_C combiner fm1 (Branch split_key elt2 _ left right)
>   = mkVBalBranch split_key new_elt 
> 		 (plusFM_C combiner lts left)
> 		 (plusFM_C combiner gts right)
>   where
>     lts     = splitLT fm1 split_key
>     gts     = splitGT fm1 split_key
>     new_elt = case lookupFM fm1 split_key of
> 		Nothing   -> elt2
> 		Just elt1 -> combiner elt1 elt2

plusFM combines two tables into a single table.  NOTE that if a key is in
both tables then the value for table two is taken.
 
> plusFM	:: Ord key => FiniteMap key elt
>                          -> FiniteMap key elt
> 			   -> FiniteMap key elt
> plusFM EmptyFM fm2 = fm2
> plusFM fm1 EmptyFM = fm1
> plusFM fm1 (Branch split_key elt1 _ left right)
>   = mkVBalBranch split_key elt1 (plusFM lts left) (plusFM gts right)
>   where
>     lts     = splitLT fm1 split_key
>     gts     = splitGT fm1 split_key

minusFM takes one table away from another table.  Only the keys from the second
table needs to be in the first table for the value to be removed.

> minusFM	:: Ord key => FiniteMap key elt
>                          -> FiniteMap key elt
>                          -> FiniteMap key elt
> minusFM EmptyFM fm2 = emptyFM
> minusFM fm1 EmptyFM = fm1
> minusFM fm1 (Branch split_key elt _ left right)
>   = glueVBal (minusFM lts left) (minusFM gts right)
>   where
>     lts = splitLT fm1 split_key	-- NB gt and lt, so the equal ones
>     gts = splitGT fm1 split_key	-- are not in either.

intersectFM takes the intersect of the two tables (this is when the keys in
both tables match).

> intersectFM	:: Ord key => FiniteMap key elt
>                          -> FiniteMap key elt
>                          -> FiniteMap key elt 
> intersectFM fm1 fm2 = intersectFM_C (\ left right -> right) fm1 fm2

intersectFM_C takes the intersect of the two tables (this is when the keys in
both tables match).  The given function is performed on the two values for
a key.

> intersectFM_C	:: Ord key => (elt -> elt -> elt)
> 			   -> FiniteMap key elt
>                          -> FiniteMap key elt
>                          -> FiniteMap key elt 
> intersectFM_C combiner fm1 EmptyFM = emptyFM
> intersectFM_C combiner EmptyFM fm2 = emptyFM
> intersectFM_C combiner fm1 (Branch split_key elt2 _ left right)
>   | isJust maybe_elt1	-- split_elt *is* in intersection
>   = mkVBalBranch split_key (combiner elt1 elt2)
>         (intersectFM_C combiner lts left) (intersectFM_C combiner gts right)
>   | otherwise			-- split_elt is *not* in intersection
>   = glueVBal
>        (intersectFM_C combiner lts left) (intersectFM_C combiner gts right)
>   where
>     lts = splitLT fm1 split_key	-- NB gt and lt, so the equal ones
>     gts = splitGT fm1 split_key	-- are not in either.
>     maybe_elt1 = lookupFM fm1 split_key
>     Just elt1  = maybe_elt1
 
 %************************************************************************
 \subsection{Mapping, folding, and filtering with @FiniteMaps@}
 %************************************************************************

> foldFM :: (key -> elt -> a -> a) -> a -> FiniteMap key elt -> a
> foldFM k z EmptyFM = z
> foldFM k z (Branch key elt _ fm_l fm_r)
>   = foldFM k (k key elt (foldFM k z fm_r)) fm_l

mapFM takes a function (of type key -> element -> element) and maps
it to each element in the table.
 
> mapFM	:: (key -> elt1 -> elt2) -> FiniteMap key elt1 -> FiniteMap key elt2
> mapFM f EmptyFM = emptyFM
> mapFM f (Branch key elt size fm_l fm_r) 
>   = Branch key (f key elt) size (mapFM f fm_l) (mapFM f fm_r)

filterFM takes a predicate and a table and filters out those elements
that do not satisfy the predicate.
 
> filterFM :: Ord key => (key -> elt -> Bool) 
> 			   -> FiniteMap key elt -> FiniteMap key elt
> filterFM p EmptyFM = emptyFM
> filterFM p (Branch key elt _ fm_l fm_r)
>   | p key elt		-- Keep the item
>   = mkVBalBranch key elt (filterFM p fm_l) (filterFM p fm_r)
>   | otherwise		-- Drop the item
>   = glueVBal (filterFM p fm_l) (filterFM p fm_r)
 
 %************************************************************************
 \subsection{Interrogating @FiniteMaps@}
 %************************************************************************

sizeFM returns the size of the table
 
> sizeFM	:: FiniteMap key elt -> Int
> sizeFM EmptyFM		= 0
> sizeFM (Branch _ _ size _ _)  = size

lookupFM takes a table and a key, and returns the corresponding element. 
If the key is not in the table a (safe) error message is returned.

> lookupFM	:: Ord key => FiniteMap key elt -> key -> Maybe elt
> lookupFM EmptyFM key = Nothing
> lookupFM (Branch key elt _ fm_l fm_r) key_to_find
>   | key_to_find < key = lookupFM fm_l key_to_find
>   | key_to_find > key = lookupFM fm_r key_to_find
>   | otherwise	        = Just elt

lookupWithDefaultFM is the same as lookupFM except a default element
is used instead of the (safe) error message.

> lookupWithDefaultFM
> 		:: Ord key => FiniteMap key elt -> elt -> key -> elt
> lookupWithDefaultFM fm deflt key_to_find
>   = case lookupFM fm key_to_find of { Nothing -> deflt; Just elt -> elt }
 
 %************************************************************************
 \subsection{Listifying @FiniteMaps@}
 %************************************************************************

fmToList takes takes a table and returns a list of key elements pairs.

> fmToList	:: FiniteMap key elt -> [(key,elt)]
> fmToList fm = foldFM (\ key elt rest -> (key,elt) : rest) [] fm

keysFM takes a table and returns a list of the keys used in the table.

> keysFM	:: FiniteMap key elt -> [key]
> keysFM fm   = foldFM (\ key elt rest -> key : rest)       [] fm

eltsFM takes a table and returns a list of the elements of the table.

> eltsFM	:: FiniteMap key elt -> [elt]
> eltsFM fm   = foldFM (\ key elt rest -> elt : rest)       [] fm
 
 %************************************************************************
 \subsection{The implementation of balancing}
 %************************************************************************
 
 %************************************************************************
 \subsubsection{Basic construction of a @FiniteMap@}
 %************************************************************************
 
> sIZE_RATIO :: Int
> sIZE_RATIO = 5
> 
> mkBranch :: Ord key 		-- Used for the assertion checking only
> 	 => Int
> 	 -> key -> elt 
> 	 -> FiniteMap key elt -> FiniteMap key elt
> 	 -> FiniteMap key elt
> 
> mkBranch which key elt fm_l fm_r
>   = --ASSERT( left_ok && right_ok && balance_ok )
>     let
> 	result = Branch key elt (unbox (1 + left_size + right_size)) fm_l fm_r
>     in
> 	result
>   where
>     left_ok  = case fm_l of
> 		EmptyFM		         -> True
> 		Branch left_key _ _ _ _  -> let
> 						biggest_left_key = fst (findMax fm_l)
> 					    in
> 					    biggest_left_key < key
>     right_ok = case fm_r of
> 		EmptyFM		         -> True
> 		Branch right_key _ _ _ _ -> let
> 						smallest_right_key = fst (findMin fm_r)
> 					    in
> 					    key < smallest_right_key
>     balance_ok = True -- sigh
>     left_size  = sizeFM fm_l
>     right_size = sizeFM fm_r
> 
>     unbox :: Int -> Int
>     unbox x = x
 
 %************************************************************************
 \subsubsection{{\em Balanced} construction of a @FiniteMap@}
 %************************************************************************
 
> mkBalBranch :: Ord key
> 	    => key -> elt 
> 	    -> FiniteMap key elt -> FiniteMap key elt
> 	    -> FiniteMap key elt
> 
> mkBalBranch key elt fm_l fm_r
> 
>   | size_l + size_r < 2 
>   = mkBranch 1{-which-} key elt fm_l fm_r
> 
>   | size_r > sIZE_RATIO * size_l	-- Right tree too big
>   = case fm_r of
> 	Branch _ _ _ fm_rl fm_rr 
> 		| sizeFM fm_rl < 2 * sizeFM fm_rr -> single_L fm_l fm_r
> 		| otherwise	   	          -> double_L fm_l fm_r
> 
>   | size_l > sIZE_RATIO * size_r	-- Left tree too big
>   = case fm_l of
> 	Branch _ _ _ fm_ll fm_lr 
> 		| sizeFM fm_lr < 2 * sizeFM fm_ll -> single_R fm_l fm_r
> 		| otherwise		          -> double_R fm_l fm_r
> 
>   | otherwise				-- No imbalance
>   = mkBranch 2{-which-} key elt fm_l fm_r
>   
>   where
>     size_l   = sizeFM fm_l
>     size_r   = sizeFM fm_r
> 
>     single_L fm_l (Branch key_r elt_r _ fm_rl fm_rr) 
> 	= mkBranch 3{-which-} key_r elt_r (mkBranch 4{-which-} key elt fm_l fm_rl) fm_rr
> 
>     double_L fm_l (Branch key_r elt_r _ (Branch key_rl elt_rl _ fm_rll fm_rlr) fm_rr)
> 	= mkBranch 5{-which-} key_rl elt_rl (mkBranch 6{-which-} key   elt   fm_l   fm_rll) 
> 				 (mkBranch 7{-which-} key_r elt_r fm_rlr fm_rr)
> 
>     single_R (Branch key_l elt_l _ fm_ll fm_lr) fm_r
> 	= mkBranch 8{-which-} key_l elt_l fm_ll (mkBranch 9{-which-} key elt fm_lr fm_r)
> 
>     double_R (Branch key_l elt_l _ fm_ll (Branch key_lr elt_lr _ fm_lrl fm_lrr)) fm_r
> 	= mkBranch 10{-which-} key_lr elt_lr (mkBranch 11{-which-} key_l elt_l fm_ll  fm_lrl)
> 				 (mkBranch 12{-which-} key   elt   fm_lrr fm_r)
> 
> 
> mkVBalBranch :: Ord key
> 	     => key -> elt 
> 	     -> FiniteMap key elt -> FiniteMap key elt
> 	     -> FiniteMap key elt
> 
> mkVBalBranch key elt EmptyFM fm_r = addToFM fm_r key elt
> mkVBalBranch key elt fm_l EmptyFM = addToFM fm_l key elt
> 
> mkVBalBranch key elt fm_l@(Branch key_l elt_l _ fm_ll fm_lr)
> 		     fm_r@(Branch key_r elt_r _ fm_rl fm_rr)
>   | sIZE_RATIO * size_l < size_r
>   = mkBalBranch key_r elt_r (mkVBalBranch key elt fm_l fm_rl) fm_rr
> 
>   | sIZE_RATIO * size_r < size_l
>   = mkBalBranch key_l elt_l fm_ll (mkVBalBranch key elt fm_lr fm_r)
> 
>   | otherwise
>   = mkBranch 13{-which-} key elt fm_l fm_r
> 
>   where 
>     size_l = sizeFM fm_l
>     size_r = sizeFM fm_r
 
 %************************************************************************
 \subsubsection{Gluing two trees together}
 %************************************************************************
 
> glueBal :: Ord key
> 	=> FiniteMap key elt -> FiniteMap key elt
> 	-> FiniteMap key elt
> 
> glueBal EmptyFM fm2 = fm2
> glueBal fm1 EmptyFM = fm1
> glueBal fm1 fm2 
>   | sizeFM fm2 > sizeFM fm1
>   = mkBalBranch mid_key2 mid_elt2 fm1 (deleteMin fm2)
> 	
>   | otherwise
>   = mkBalBranch mid_key1 mid_elt1 (deleteMax fm1) fm2
>   where
>     (mid_key1, mid_elt1) = findMax fm1
>     (mid_key2, mid_elt2) = findMin fm2

> glueVBal :: Ord key
> 	 => FiniteMap key elt -> FiniteMap key elt
> 	 -> FiniteMap key elt
> 
> glueVBal EmptyFM fm2 = fm2
> glueVBal fm1 EmptyFM = fm1
> glueVBal fm_l@(Branch key_l elt_l _ fm_ll fm_lr)
>          fm_r@(Branch key_r elt_r _ fm_rl fm_rr)
>   | sIZE_RATIO * size_l < size_r
>   = mkBalBranch key_r elt_r (glueVBal fm_l fm_rl) fm_rr
> 
>   | sIZE_RATIO * size_r < size_l
>   = mkBalBranch key_l elt_l fm_ll (glueVBal fm_lr fm_r)
> 
>   | otherwise		-- We now need the same two cases as in glueBal above.
>   = glueBal fm_l fm_r
>   where
>     (mid_key_l,mid_elt_l) = findMax fm_l
>     (mid_key_r,mid_elt_r) = findMin fm_r
>     size_l = sizeFM fm_l
>     size_r = sizeFM fm_r
 
 %************************************************************************
 \subsection{Local utilities}
 %************************************************************************
 
> splitLT, splitGT :: Ord key => FiniteMap key elt -> key -> FiniteMap key elt
> 
> splitLT EmptyFM split_key = emptyFM
> splitLT (Branch key elt _ fm_l fm_r) split_key
>   | split_key < key = splitLT fm_l split_key
>   | split_key > key = mkVBalBranch key elt fm_l (splitLT fm_r split_key)
>   | otherwise	    = fm_l
> 
> splitGT EmptyFM split_key = emptyFM
> splitGT (Branch key elt _ fm_l fm_r) split_key
>   | split_key > key = splitGT fm_r split_key
>   | split_key < key = mkVBalBranch key elt (splitGT fm_l split_key) fm_r
>   | otherwise	    = fm_r
> 
> findMin :: FiniteMap key elt -> (key,elt)
> findMin (Branch key elt _ EmptyFM _) = (key,elt)
> findMin (Branch key elt _ fm_l    _) = findMin fm_l
> 
> deleteMin :: Ord key => FiniteMap key elt -> FiniteMap key elt
> deleteMin (Branch key elt _ EmptyFM fm_r) = fm_r
> deleteMin (Branch key elt _ fm_l    fm_r) = mkBalBranch key elt (deleteMin fm_l) fm_r
> 
> findMax :: FiniteMap key elt -> (key,elt)
> findMax (Branch key elt _ _ EmptyFM) = (key,elt)
> findMax (Branch key elt _ _    fm_r) = findMax fm_r
> 
> deleteMax :: Ord key => FiniteMap key elt -> FiniteMap key elt
> deleteMax (Branch key elt _ fm_l EmptyFM) = fm_l
> deleteMax (Branch key elt _ fm_l    fm_r) = mkBalBranch key elt fm_l (deleteMax fm_r)
 
