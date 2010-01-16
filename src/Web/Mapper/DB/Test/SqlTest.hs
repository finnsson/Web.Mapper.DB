{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell #-}
module Web.Mapper.DB.Test.SqlTest where

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
import TestGenerator

main = defaultMain [sqlTest] 

sqlTest = $testGroupGenerator

-- Fixtures
boolPrimInfoFixture = PrimInfo "bool" "pg_catalog"
integerPrimInfoFixture = PrimInfo "int4" "pg_catalog"
primTypeInfoFixture = [boolPrimInfoFixture, integerPrimInfoFixture]
dbArgumentFixture = [DbArgument "foo" 0 "bool" "pg_catalog" "boolean_to_integer" "public"]
dbFunctionFixture = [DbFunction 100 "boolean_to_integer" "public" "Integer" "pg_catalog" (Just "My super comment.")]

-- Helpers
typeInfo (MetaInfo t _ ) = t
procInfo (MetaInfo _ p ) = p

connectionString = "dbname=SqlTest user=test password=test"

test_functions =
  do conn <- connectPostgreSQL connectionString
     functions <- pgCall sqlFunctions sql2DbFunction conn
     1 @=? (length functions)
     let f = head functions
     "int4" @=? (dbFunctionTypeName f)
     "pg_catalog" @=? (dbFunctionTypeNS f)
     
test_arguments =
  do conn <- connectPostgreSQL connectionString
     arguments <- pgCall sqlArguments sql2DbArgument conn
     1 @=? (length arguments)
     let f = head arguments
     "bool" @=? (dbArgumentTypeName f)
     "pg_catalog" @=? (dbArgumentTypeNS f)
     


