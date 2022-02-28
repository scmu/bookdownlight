{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cheapskate.Types where
import Data.Sequence (Seq)
import Data.Default
import Data.Text (Text)
import qualified Data.Map as M
import Data.Data
import Control.DeepSeq (NFData(..))
import GHC.Generics (Generic)

-- | Structured representation of a document.  The 'Options' affect
-- how the document is rendered by `toHtml`.
data Doc = Doc Options Blocks
           deriving (Show, Data, Typeable)

-- | Block-level elements.
data Block = Para Inlines
           | Header Int [Attr] Inlines
           | Blockquote Blocks
           | List Bool ListType [Blocks]
           | CodeBlock [Attr] Text
           | HtmlBlock Text
           | HRule
           | DIV [Attr] Blocks
           deriving (Show, Data, Typeable)

-- | Attributes for fenced code blocks.  'codeLang' is the
-- first word of the attribute line, 'codeInfo' is the rest.
data CodeAttr = CodeAttr { codeLang :: Text, codeInfo :: Text }
              deriving (Show, Data, Typeable)

data ListType = Bullet Char | Numbered NumWrapper Int deriving (Eq,Show,Data,Typeable)

data NumWrapper = PeriodFollowing | ParenFollowing deriving (Eq,Show,Data,Typeable)

-- | Simple representation of HTML tag.
data HtmlTagType = Opening Text | Closing Text | SelfClosing Text deriving (Show, Data, Typeable)

-- We operate with sequences instead of lists, because
-- they allow more efficient appending on to the end.
type Blocks = Seq Block

-- | Inline elements.
data Inline = Str Text
            | Space
            | SoftBreak
            | LineBreak
            | Emph Inlines
            | Strong Inlines
            | Code Text
            | HsCode Text
            | Link Inlines Text {- URL -} Text {- title -}
            | Image Inlines Text {- URL -} Text {- title -}
            | Entity Text
            | RawHtml Text
            | Attrs [Attr]
            | Footnote Inlines
            | Index Text
            | Ref Text
            | EqRef Text
            | CiteP [(Text, Maybe Text)]  -- (citation, options)
            | CiteT Text (Maybe Text)     -- Name (year, options)
            deriving (Show, Data, Typeable)

type Inlines = Seq Inline

type ReferenceMap = M.Map Text (Text, Text)

-- | Rendering and parsing options.
data Options = Options{
    sanitize           :: Bool  -- ^ Sanitize raw HTML, link/image attributes
  , allowRawHtml       :: Bool  -- ^ Allow raw HTML (if false it gets escaped)
  , preserveHardBreaks :: Bool  -- ^ Preserve hard line breaks in the source
  , debug              :: Bool  -- ^ Print container structure for debugging
  }
  deriving (Show, Data, Typeable)

instance Default Options where
  def = Options{
          sanitize = True
        , allowRawHtml = True
        , preserveHardBreaks = False
        , debug = False
        }

data Attr = AtrClass Text    -- .class
          | AtrID Text       -- #id
          | Atr Text Text    -- attr="val"
  deriving (Show, Data, Typeable, Eq)

isAtrId (AtrID _) = True
isAtrId _         = False
unAtrID (AtrID xs) = xs

isAtrCls (AtrClass _) = True
isAtrCls _            = False
unAtrClass (AtrClass xs) = xs

isAtrAV (Atr _ _) = True
isAtrAV _         = False
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

deriving instance Generic Doc
instance NFData Doc

deriving instance Generic Block
instance NFData Block

deriving instance Generic CodeAttr
instance NFData CodeAttr

deriving instance Generic ListType
instance NFData ListType

deriving instance Generic NumWrapper
instance NFData NumWrapper

deriving instance Generic HtmlTagType
instance NFData HtmlTagType

deriving instance Generic Inline
instance NFData Inline

deriving instance Generic Options
instance NFData Options

deriving instance Generic Attr
instance NFData Attr
