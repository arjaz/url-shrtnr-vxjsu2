module Tree.TreePropertiesSpec (spec) where

import Data.Text ()
import qualified Data.Text as Text
import qualified Database.Tree.Tree as BT
import qualified Database.Url.Url as Url
import Relude
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "BT.singleton" $ do

    context "when created" $ do
      prop "should have only the element it was created with" $
        \(originString, shortenedString) -> do
          let origin = Text.pack originString
              short = Text.pack shortenedString
              url = Url.Url origin short
          BT.toList (BT.singleton url) `shouldBe` [(short, url)]

    context "when have its element deleted" $ do
      prop "should have no elements in it" $
        \(originString, shortenedString) -> do
          let origin = Text.pack originString
              short = Text.pack shortenedString
              url = Url.Url origin short
          BT.toList (BT.delete short (BT.singleton url)) `shouldBe` []
