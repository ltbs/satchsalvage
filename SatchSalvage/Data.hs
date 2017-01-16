module SatchSalvage.Data where

type ProductAttr = (String, String)
type Price = (Int, Double)

data Product = Product {
  prodId :: Int,
  sku :: String,
  name :: String,
  slug :: String,
  desc :: String,
  descShort :: Maybe String,
  active :: Bool,
  attributes :: [ProductAttr],
  prices :: [Price],
  parentSku :: Maybe String
  } deriving Show

data Category = Category { satchmoCatId :: Int
                         , catName :: String
                         , catSlug :: String
                         }
