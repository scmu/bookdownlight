module Syntax.Util where

import Data.Text (Text)
import Cheapskate.Types

isAtrId (AtrID _) = True
isAtrId _         = False
isThisId id0 (AtrID id1) | id0 == id1 = True
isThisId _ _      = False
unAtrID (AtrID xs) = xs

isAtrCls (AtrClass _) = True
isAtrCls _            = False
isThisCls cl0 (AtrClass cl1) | cl0 == cl1 = True
isThisCls _ _         = False
unAtrClass (AtrClass xs) = xs

isAtrAV (Atr _ _) = True
isAtrAV _         = False
isThisAtr at0 (Atr at1 _) | at0 == at1 = True
isThisAtr _ _     = False
unAtrAv (Atr atr val) = (atr, val)

attrsId :: [Attr] -> [Text]
attrsId = map unAtrID . filter isAtrId

attrsClass :: [Attr] -> [Text]
attrsClass = map unAtrClass . filter isAtrCls

attrsAVs :: [Attr] -> [(Text, Text)]
attrsAVs = map unAtrAv . filter isAtrAV

lookupAttrs :: Text -> [Attr] -> Maybe Text
lookupAttrs atr [] = Nothing
lookupAttrs atr (Atr atr' val : attrs)
   | atr == atr' = Just val
   | otherwise   = lookupAttrs atr attrs
lookupAttrs atr (_ : attrs) = lookupAttrs atr attrs

hasClass :: Text -> [Attr] -> Bool
hasClass cls attrs = AtrClass cls `elem` attrs

isAttrs :: Inline -> Bool
isAttrs (Attrs _) = True
isAttrs _ = False
unAttrs (Attrs attrs) = attrs

isHsCode :: Inline -> Bool
isHsCode (HsCode _) = True
isHsCode _ = False
unHsCode (HsCode txt) = txt
