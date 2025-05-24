{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module Web.Atomic.Html where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Writer
import Data.Functor.Identity (Identity (..))
import Data.List qualified as L
import Data.Map.Strict (Map)
import Data.String (IsString (..))
import Data.Text (Text, pack)
import GHC.Exts (IsList (..))
import Web.Atomic.Types

-- | Html monad
--
-- @
-- import Web.Atomic
--
-- example = do
--  'el' ~ pad 10 $ do
--    'el' ~ fontSize 24 . bold $ "My Links"
--    a '@' href "hoogle.haskell.org" ~ link $ \"Hoogle\"
--    a '@' href "hackage.haskell.org" ~ link $ \"Hackage\"
--
-- link = underline . color Primary
-- a = 'tag' "a"
-- href = 'att' "href"
-- @
newtype HtmlT m a = HtmlT (WriterT [Node] m a) deriving newtype (Functor, Applicative, Monad, MonadIO, MonadFail, MonadPlus, Alternative, MonadTrans)

runHtmlT :: HtmlT m a -> m (a, [Node])
runHtmlT (HtmlT m) = runWriterT m

type Html = HtmlT Identity

instance (Monad m) => IsString (HtmlT m ()) where
  fromString s = HtmlT $ do
    tell [fromString s]

el :: Html () -> Html ()
el = tag "div"

tag :: (Monad m) => Text -> HtmlT m a -> HtmlT m a
tag nm (HtmlT m) = HtmlT $ do
  (a, nodes) <- lift $ runWriterT m
  tell [Elem $ (element nm) {content = nodes}]
  pure a

text :: (Monad m) => Text -> HtmlT m ()
text t = HtmlT $ tell $ [Text t]

none :: (Monad m) => HtmlT m ()
none = pure ()

raw :: (Monad m) => Text -> HtmlT m ()
raw t = HtmlT $ tell [Raw t]

-- | A single 'Html' element. Note that the class attribute is generated separately from the css rules, rather than the attributes
data Element = Element
  { inline :: Bool,
    name :: Text,
    css :: [Rule],
    attributes :: Map Name AttValue,
    content :: [Node]
  }

data Node
  = Elem Element
  | Text Text
  | Raw Text

instance IsString Node where
  fromString s = Text (pack s)

mapElement :: (Element -> Element) -> Html a -> Html a
mapElement f (HtmlT m) = HtmlT $ do
  (a, nodes) <- lift $ runWriterT m
  tell $ fmap (mapNodeElement f) nodes
  pure a

mapNodeElement :: (Element -> Element) -> Node -> Node
mapNodeElement f (Elem e) = Elem $ f e
mapNodeElement _ n = n

element :: Text -> Element
element nm = Element False nm mempty mempty mempty

instance Attributable (Html a) where
  modAttributes f =
    mapElement (\elm -> elm {attributes = f elm.attributes})

instance Styleable (Html a) where
  modCSS f =
    mapElement (\elm -> elm {css = f elm.css})

htmlCSSRules :: (Monad m) => HtmlT m a -> m (Map Selector Rule)
htmlCSSRules (HtmlT m) = do
  (_, nodes) <- runWriterT m
  pure $ mconcat $ fmap nodeCSSRules nodes

nodeCSSRules :: Node -> Map Selector Rule
nodeCSSRules = \case
  Elem elm -> elementCSSRules elm
  _ -> []

elementCSSRules :: Element -> Map Selector Rule
elementCSSRules elm =
  ruleMap elm.css <> mconcat (fmap nodeCSSRules elm.content)

elementClasses :: Element -> [ClassName]
elementClasses elm =
  L.sort $ fmap ruleClassName elm.css
