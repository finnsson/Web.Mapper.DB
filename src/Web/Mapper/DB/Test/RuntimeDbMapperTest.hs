{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell #-}
module Web.Mapper.DB.Test.RuntimeDbMapperTest where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import Data.List
import Data.Char
import Database.HDBC
import Database.HDBC.PostgreSQL
import Maybe
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Control.Exception as E

import TestGenerator
import Web.Mapper.DB.RuntimeDbMapper
import Web.Mapper.Mapper
import Web.Mapper.DB.Meta

import Utilities.Misc

main = defaultMain [runtimeDbMapperTest] 

runtimeDbMapperTest = $testGroupGenerator

-- Fixtures
name = "foo"
whereParams = [("arg1","val1"),("arg2","val2")]
valueParams = [("argA","valA"),("argB","valB")]
cs = "dbname=RuntimeDbMapperTest user=test password=test"

-- Select

testSelectSql =
  do let actual = selectSql name whereParams
         expected = "select * from foo where arg1=? and arg2=?"
     expected @=? actual

testSelectSqlEmpty =
  do let params = []
         actual = selectSql name params
         expected = "select * from foo"
     expected @=? actual

testSelect =
  do actual <- select' cs (DataInput Read False "xml" "public" "int_int" [] [])
     let expected = MapperOutput [[("fst","11"), ("snd","22")]]
     expected @=? actual

testSelectWithFilter =
  do actual <- select' cs (dataInput {dataInputVerb = Read, dataInputNS = "public", dataInputName = "int_int", dataInputValue = [], dataInputFilter = [("fst","11")]})
     let expected = MapperOutput [[("fst","11"), ("snd","22")]]
     expected @=? actual

testSelectWithNegativeFilter =
  do actual <- select' cs (dataInput {dataInputVerb = Read, dataInputNS = "public", dataInputName = "int_int", dataInputValue = [], dataInputFilter = [("fst","12")]})
     let expected = MapperOutput []
     expected @=? actual

testFilter =
  do let actual = map ( sqlInjectionProtection . fst) [("fst","11")]
         expected = [Nothing] :: [Maybe String]
     expected @=? actual

-- Insert

testInsertSql =
  do let actual = insertSql name valueParams
         expected = "insert into foo(argA,argB) values(?,?)"
     expected @=? actual

-- Master Test

testInsertUpdateDeleteSql =
  do let di = (DataInput Delete False "xml" "public" "test_test" [] [])
     -- clean table
     rowsCleaned <- delete' cs di { dataInputVerb = Delete }
     -- verify table is cleaned
     rowsAfterClean <- select' cs di { dataInputVerb = Read }
     (MapperOutput [] ) @=? rowsAfterClean

     -- insert row
     rowInserted <- insert' cs di { 
       dataInputVerb = Create, 
       dataInputValue = [("fst","1"),("snd","3")] }
     -- verify row is inserted
     selectAfterInsert <- select' cs di { dataInputVerb = Read }
     (MapperOutput [[("fst","1"),("snd","3")]]) @=? selectAfterInsert
     -- update row
     rowUpdated <- update' cs di { 
       dataInputVerb = Update,
       dataInputValue = [("fst","2")],
       dataInputFilter = [("snd","3")]}
     -- verify row is updated
     rowsAfterUpdate <- select' cs di { dataInputVerb = Read }
     (MapperOutput [[("fst","2"),("snd","3")]]) @=? rowsAfterUpdate

     -- delete row
     rowsDeleted <- delete' cs di { dataInputVerb = Delete }
     -- verify row is deleted
     rowsAfterFinalDelete <- select' cs di { dataInputVerb = Read }
     (MapperOutput [] ) @=? rowsAfterFinalDelete

-- Update

testUpdateSql =
  do let actual = updateSql name [] valueParams
         expected = "update foo set argA=?, argB=?"
     expected @=? actual

testUpdateSqlWithWhere =
  do let actual = updateSql name whereParams valueParams
         expected = "update foo set argA=?, argB=? where arg1=? and arg2=?"
     expected @=? actual


-- Delete
testDeleteSql =
  do let actual = deleteSql name whereParams
         expected = "delete from foo where arg1=? and arg2=?"
     expected @=? actual

-- Exec
testMethodSql =
  do let actual = methodSql name valueParams
         expected = "exec foo valA valB"
     expected @=? actual

-- Meta
testMetaRoot =
  do actual <- getMapperOutput runtimeDbMapper dataInput
     let expected = MapperOutputMeta $ metaInfo -- [] []
     expected @=? actual
  where
     runtimeDbMapper = RuntimeDbMapper cs ["public"] []
     dataInput = DataInput Read True "xml" "" "" [] []

testDbInfoRoot =
  do actual <- dbInfo cs
     let expected = metaInfo
     expected @=? actual

metaInfo =
  MetaInfo [
    TableInfo "int_int" "public" 
      [ColumnInfo "fst" (PrimInfo "int4" "pg_catalog"),ColumnInfo "snd" (PrimInfo "int4" "pg_catalog")] 
      [(SelectPrivilege,True),(UpdatePrivilege,True),(InsertPrivilege,True),(DeletePrivilege,True)],
    TableInfo "oid_array" "public" 
      [ColumnInfo "array" (PrimInfo "_int4" "pg_catalog"),ColumnInfo "id" (PrimInfo "oid" "pg_catalog")] 
      [(SelectPrivilege,True),(UpdatePrivilege,True),(InsertPrivilege,True),(DeletePrivilege,True)],
    TableInfo "test_test" "public" 
      [ColumnInfo "fst" (PrimInfo "int4" "pg_catalog"),ColumnInfo "snd" (PrimInfo "int4" "pg_catalog")] 
      [(SelectPrivilege,True),(UpdatePrivilege,True),(InsertPrivilege,True),(DeletePrivilege,True)],
    TableInfo "bool_int_int" "public" 
      [
      ColumnInfo "oid" (PrimInfo "oid" "pg_catalog"),
      ColumnInfo "fst" (PrimInfo "bit" "pg_catalog"),
      ColumnInfo "snd" (TableInfo "int_int" "public"
        [
        ColumnInfo "fst" (PrimInfo "int4" "pg_catalog"),
        ColumnInfo "snd" (PrimInfo "int4" "pg_catalog")
        ] 
        [(SelectPrivilege,True),(UpdatePrivilege,True),(InsertPrivilege,True),(DeletePrivilege,True)])
      ] 
      [(SelectPrivilege,True),(UpdatePrivilege,True),(InsertPrivilege,True),(DeletePrivilege,True)]
    ]
    []

-- Test Helpers

testSelectSqlInjection =
  do let actual1 = isJust $ sqlInjectionProtection "[ "
         expected1 = True
     expected1 @=? actual1



