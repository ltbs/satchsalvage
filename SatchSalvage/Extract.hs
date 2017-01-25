{-# LANGUAGE OverloadedStrings #-}

module SatchSalvage.Extract where

import SatchSalvage.Data
import SatchSalvage.Convert

import Database.MySQL.Simple
import qualified Data.Text as Text
import System.Posix.Files
import System.Posix.Directory
import Data.Maybe (fromMaybe)
import System.FilePath.Posix
import Data.List (stripPrefix)

stripStart :: Eq a => [a] -> [a] -> [a]
stripStart r v = fromMaybe v (stripPrefix r v)

replace :: Eq b => b -> b -> [b] -> [b]
replace a b = map (\x -> if a == x then b else x)

sanitise :: String -> String
sanitise = replace '/' '_' 

display :: Text.Text -> IO ()
display = putStrLn.Text.unpack

categories :: Connection -> IO [Category]
categories c = raw >>= mapM loadCat
  where
    raw = query_ c "SELECT id, name, slug FROM product_category WHERE is_active = 1"
    loadCat :: (Int, String, String) -> IO Category
    loadCat (idR, nameR, slugR) = return $ Category idR nameR slugR

products :: Connection -> IO [Product]
products c = raw >>= mapM loadProduct
  where
    raw = query_ c sql
    sql = "SELECT p.id, p.sku, p.name, p.slug, p.description, p.short_description, p.active, p2.sku as parent_sku FROM product_product p LEFT JOIN configurable_productvariation v ON (v.product_id = p.id) LEFT JOIN product_product p2 ON (v.parent_id = p2.id)"
    loadProduct :: (Int, String, String, String, String, String, Int, Maybe String) -> IO Product
    loadProduct (idR, skuR, nameR, slugR, descR, descS, enabled, parent) = do 
      attrs <- productAttributes c idR
      priceData <- productPrices c idR      
      return Product {
        prodId = idR,
        sku = sanitise skuR,
        name = nameR,
        slug = sanitise slugR,
        desc = descR,
        descShort = if null descS then Nothing else Just descS,
        active = enabled == 1,
        prices = priceData,
        attributes = attrs,
        parentSku = sanitise <$> parent
      }

productPrices :: Connection -> Int -> IO [Price]
productPrices conn pid = query conn sql (Only pid) >>= return.fmap fromStupid
  where fromStupid :: (Double, Double) -> Price
        fromStupid (k,v) = (floor k, v)
        sql = "SELECT quantity, price FROM product_price WHERE product_id = ?"

productAttributes :: Connection -> Int -> IO [ProductAttr]
productAttributes conn pid = query conn sql (Only pid)
  where sql = "SELECT description, value FROM product_productattribute pa, product_attributeoption ao WHERE pa.option_id = ao.id AND product_id = ?"

productDir :: Product -> FilePath
productDir p = baseDir ++ fromMaybe "" (("/" ++) <$> parentSku p) ++ "/"
  where baseDir = "products/" ++ if active p then "active/" else "inactive/"

getProductImagePaths :: Connection -> Product -> IO [FilePath]
getProductImagePaths c p = query c sql (Only (prodId p))  >>= return.fmap fromOnly
  where sql = "SELECT picture FROM product_productimage WHERE product_id = ?"

copyImages :: FilePath -> Connection -> Product -> IO ()
copyImages staticDir c p = do
  images <- getProductImagePaths c p
  mapM_ (\i -> createLink (staticDir ++ "/" ++ i) (destPath i)) images
  where destPath x = productDir p ++ sku p ++ "/" ++
          (stripStart "productimage-picture-" .takeFileName) x        

makeProductFs :: Product -> IO ()
makeProductFs p = do
  createDirectory (productDir p ++ sku p) ownerModes
  createSymbolicLink (sku p) (productDir p ++ slug p)
  _ <- htmlToMdIO (desc p) >>= writeFile (productDir p ++ sku p ++ "/desc-en_GB.md")
  writeFile (productDir p ++ sku p ++ "/prices") priceData  
  writeFile (productDir p ++ sku p ++ "/" ++ slug p ++ ".product") metadata
  where metadata = unlines $ [
          "title: " ++ name p,
          "active: " ++ show (active p)
          ] ++ attr
        attr = fmap (\(k,v) -> k ++ ": " ++ v) (attributes p)
        priceData = (unlines.fmap (\(k,v) -> show k ++ ":" ++ show v ++ "GBP")) (prices p)

makeCategoryFs :: Connection -> Category -> IO ()
makeCategoryFs con cat = do
  createDirectory catDir ownerModes
  writeFile (catDir ++ "/" ++ catSlug cat ++ ".category") metadata
  catProdSlugs >>= mapM_ makeSlugLink
  where metadata = "title: " ++ catName cat
        catDir = "categories/" ++ catSlug cat
        catProdSlugs :: IO [(String, Int)]
        catProdSlugs = query con sql (Only (satchmoCatId cat))
        sql = "SELECT p.slug, p.active FROM product_product_category c JOIN product_product p ON (c.product_id = p.id) WHERE category_id = ?"
        makeSlugLink (newSlug, 1) = createSymbolicLink ("../../products/active/" ++ newSlug) (catDir ++ "/" ++ newSlug)
        makeSlugLink (_, _) = return ()

makeSkeleton :: IO ()
makeSkeleton = mapM_ (`createDirectory` ownerModes) dirs 
  where dirs = ["products", "products/active", "products/inactive", "categories"]
