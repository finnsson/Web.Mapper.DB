{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell #-}
module Web.Mapper.DB.Test.DbSimpleTest where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import Test.HUnit

import Data.List
import Data.Char
import Database.HDBC
import Database.HDBC.PostgreSQL

import Web.Mapper.Mapper
import Web.Mapper.DB.Meta
import Web.Mapper.DB.Sql

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Utilities.Misc
import TestGenerator

main = defaultMain [dbSimpleTest] 

dbSimpleTest = $testGroupGenerator

-- Fixtures
boolPrimInfoFixture = PrimInfo "bool" "pg_catalog"
integerPrimInfoFixture = PrimInfo "int4" "pg_catalog"
stringPrimInfoFixture = PrimInfo "String" "pg_catalog"
primTypeInfoFixture = [boolPrimInfoFixture, integerPrimInfoFixture]
dbArgumentFixture = [DbArgument "foo" 0 "bool" "pg_catalog" "boolean_to_integer" "public"]
dbFunctionFixture = [DbFunction 100 "boolean_to_integer" "public" "Integer" "pg_catalog" (Just "My super comment.")]

boolBoolTableInfoFixture =
  TableInfo "bool_bool" "public" [ColumnInfo "fst" boolPrimInfoFixture, ColumnInfo "snd" boolPrimInfoFixture] allPrivileges

allPrivileges = [(SelectPrivilege,True),(UpdatePrivilege,True),(InsertPrivilege,True),(DeletePrivilege,True)]

procInfoFixture =
          ProcInfo {
            procInfoReturnType = integerPrimInfoFixture,
            procInfoName = "boolean_to_integer",
            procInfoNS = "public",
            procInfoArguments = [("foo",boolPrimInfoFixture)],
            procInfoComment = Just "My super comment."
          }


-- Helpers
typeInfo (MetaInfo t _ ) = t
procInfo (MetaInfo _ p ) = p

test_resolveTableGraph = ([],[]) @=? resolveTableGraph [] []

test_dbInfoLength =
  do actual <- dbInfo connectionString 
     1 @=? ( length $ typeInfo actual )
     1 @=? ( length $ procInfo actual )

test_sql2String = "Hej" @=? sql2String (SqlString "Hej")

test_sql2Integer_1 = 42 @=? sql2Integer (SqlInteger 42)

test_sql2Integer_2 = 42 @=? sql2Integer (SqlString "42")

test_picker_1 = "Foo" @=? picker [SqlString "Foo", SqlString "Hej"] 0

test_picker_2 = "Hej" @=? picker [SqlString "Foo", SqlString "Hej"] 1


connectionString = "dbname=DbTestSimple user=test password=test"

-- Test procs

test_pgProc =
  do conn <- connectPostgreSQL connectionString
     procs <- pgProc primTypeInfoFixture conn
     1 @=? length procs
     let actual = procs !! 0
         expected =
           ProcInfo {
            procInfoReturnType = integerPrimInfoFixture,
            procInfoName = "boolean_to_integer",
            procInfoNS = "public",
            procInfoArguments = [("foo",boolPrimInfoFixture)],
            procInfoComment = Just "My super comment."
           }
     expected @=? actual

test_pgFunctions = 
  do conn <- connectPostgreSQL connectionString
     actual <- (pgCall sqlFunctions sql2DbFunction conn) >>* (!! 0)
     let expected = DbFunction (dbFunctionId actual) "boolean_to_integer" "public" "int4" "pg_catalog" (Just "My super comment.")
     expected @=? actual

test_getProcInfos =  1 @=? (length $ getProcInfos primTypeInfoFixture dbArgumentFixture dbFunctionFixture )


-- Test tables

test_pgType =
  do conn <- connectPostgreSQL connectionString
     types <- pgType conn
     1 @=? (length types - length primInfos)
     boolBoolTableInfoFixture @=? (last types)

test_dbInfo =
  do actual <- dbInfo connectionString
     let expected = MetaInfo [boolBoolTableInfoFixture] [procInfoFixture]
     expected @=? actual
