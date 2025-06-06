module Test.StyleSpec (spec) where

import Skeletest
import Web.Atomic.CSS
import Web.Atomic.Types
import Prelude hiding (span)


spec :: Spec
spec = do
  mainSpec
  selectorSpec


mainSpec :: Spec
mainSpec = do
  describe "PropertyStyle" $ do
    it "should compile, and set both the className and styles" $ do
      let rs = rules $ list Decimal
      length rs `shouldBe` 1
      [c] <- pure rs
      ruleClassName c `shouldBe` ClassName "list-decimal"
      ruleSelector c `shouldBe` ".list-decimal"
      c.properties `shouldBe` ["list-style-type" :. "decimal"]

    it "should work with outside member None" $ do
      let rs = rules $ list None
      length rs `shouldBe` 1
      [c] <- pure rs
      ruleClassName c `shouldBe` ClassName "list-none"
      ruleSelector c `shouldBe` ".list-none"
      c.properties `shouldBe` ["list-style-type" :. "none"]

  describe "PxRem" $ do
    it "uses absolutes for 0,1" $ do
      style (PxRem 0) `shouldBe` "0px"
      style (PxRem 16) `shouldBe` "1.000rem"

    it "uses rem for others" $ do
      style (PxRem 2) `shouldBe` "0.125rem"
      style (PxRem 10) `shouldBe` "0.625rem"
      style (PxRem 16) `shouldBe` "1.000rem"

  describe "Length" $ do
    it "styles pct" $ do
      style (Pct (1 / 3)) `shouldBe` "33.3%"

    it "adds values" $ do
      style (PxRem 6 + PxRem 10) `shouldBe` "1.000rem"

  describe "Align" $ do
    it "should produce correct style values" $ do
      style AlignCenter `shouldBe` "center"
      style AlignJustify `shouldBe` "justify"

  describe "ToClassName" $ do
    it "should hyphenate classnames" $ do
      "woot" -. None `shouldBe` "woot-none"

    it "should not hyphenate with empty suffix" $ do
      "woot" -. () `shouldBe` "woot"

    it "should escape classNames" $ do
      className "hello.woot-hi" `shouldBe` ClassName "hello-woot-hi"

  describe "Colors" $ do
    it "correct styleValue independent of leading slash" $ do
      style (HexColor "#FFF") `shouldBe` Style "#FFF"
      style (HexColor "FFF") `shouldBe` Style "#FFF"
      style ("FFF" :: HexColor) `shouldBe` Style "#FFF"
      style ("#FFF" :: HexColor) `shouldBe` Style "#FFF"

    it "correct className independent of leading slash" $ do
      toClassName (HexColor "#FFF") `shouldBe` "fff"
      toClassName (HexColor "FFF") `shouldBe` "fff"
      toClassName ("FFF" :: HexColor) `shouldBe` "fff"
      toClassName ("#FFF" :: HexColor) `shouldBe` "fff"

    it "works with custom colors" $ do
      style (colorValue Danger) `shouldBe` Style "#F00"
      style (colorValue Warning) `shouldBe` Style "#FF0"

  describe "Styleable" $ do
    it "applies styles" $ do
      let rs :: [Rule] = [] ~ bold . fontSize 24
      fmap (.className) rs `shouldBe` ["bold", "fs-24"]

    it "writes in composition order" $ do
      let rs :: [Rule] = [] ~ bold . fontSize 12 . italic
      fmap (.className) rs `shouldBe` ["bold", "fs-12", "italic"]

    it "overrides in operator order" $ do
      let rs :: [Rule] = [] ~ bold . fontSize 12 ~ italic
      fmap (.className) rs `shouldBe` ["italic", "bold", "fs-12"]

  describe "External Classes" $ do
    it "adds external classes" $ do
      let CSS rs = CSS [] ~ cls "external"
      rs `shouldBe` [Rule "external" mempty mempty []]
      fmap (.className) rs `shouldBe` ["external"]


selectorSpec :: Spec
selectorSpec = do
  describe "Selector" $ do
    it "normal selector" $ do
      selector "myclass" `shouldBe` Selector ".myclass"

    it "escapes colons" $ do
      selector "hover:bold" `shouldBe` Selector ".hover\\:bold"


data AppColor
  = Danger
  | Warning
  deriving (Show, Eq)


instance ToColor AppColor where
  colorValue Danger = "#F00"
  colorValue Warning = "FF0"
