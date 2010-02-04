{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell #-}
module Web.Mapper.DB.Test.MetaCRUDTest where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import Test.HUnit

import Data.List
import Data.Char
import Database.HDBC
import Database.HDBC.PostgreSQL

import Web.Mapper.Mapper

import Web.Mapper.DB.MetaCRUD

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Utilities.Misc
import TestGenerator

main = defaultMain [metaCRUDTest] 

metaCRUDTest = $testGroupGenerator

cs = "dbname=MetaCRUDTest user=test password=test"

testCreateEmptyTableAndDropIt = do
  let di = dataInput { dataInputNS = "public", dataInputName = "foo" }
  -- clean up namespace/table
  _ <- metaDelete cs di
  actual <- metaCreate cs di
  let expected = MapperOutput []
  expected @=? actual
  actualDelete <- metaDelete cs di
  let expectedDelete = MapperOutput []
  expectedDelete @=? actualDelete

testCreateColumnAndDropIt = do
  let diTable = dataInput { dataInputNS = "public", dataInputName = "foo" }
      diColumn = diTable { dataInputValue = [("bar","int4")] }
  _ <- metaDelete cs diTable
  let expected = MapperOutput []
  actualTable <- metaCreate cs diTable -- create table foo
  expected @=? actualTable
  actualColumn <- metaCreate cs diColumn -- create column bar
  expected @=? actualColumn
  actualDelete <- metaDelete cs diColumn -- delete column bar
  expected @=? actualDelete
  actualDelete' <- metaDelete cs diTable -- delete table foo
  expected @=? actualDelete'
