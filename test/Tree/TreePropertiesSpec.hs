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

  describe "BT.insert && BT.delete && BT.lookup" $ do
      context "when insert element" $ do
        prop "should have this element from lookup" $
          \(pairs, originString, shortenedString) -> do
            let origin = Text.pack originString
                short = Text.pack shortenedString
                url = Url.Url origin short
                newPairs = map (bimap Text.pack Text.pack) pairs
                urls = map (uncurry Url.Url) newPairs
                tree = BT.fromList [(Url.short url', url') | url' <- urls]
            BT.lookup short (BT.insert url tree) `shouldBe` Just url

      context "when deleting element" $ do
        prop "shouldn't have this element from lookup" $
          \(pairs, shortenedString) -> do
            let short = Text.pack shortenedString
                newPairs = map (bimap Text.pack Text.pack) pairs
                urls = map (uncurry Url.Url) newPairs
                tree = BT.fromList [(Url.short url', url') | url' <- urls]
            BT.lookup short (BT.delete short tree) `shouldBe` Nothing
