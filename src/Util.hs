module Util where

import Parse (Atom (..))

variant :: Atom -> Atom -> Bool
variant (AInt    _) (AInt    _) = True
variant (AFloat  _) (AFloat  _) = True
variant (AString _) (AString _) = True
variant (ABool   _) (ABool   _) = True
variant (AList   _) (AList   _) = True
variant _ _ = False

sameVariant :: [Atom] -> Bool
sameVariant [] = True
sameVariant (x:xs) = all (variant x) xs
