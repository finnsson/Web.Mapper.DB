{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell #-}
module Web.Mapper.DB.Test.DbPrivilegeTest where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import Test.HUnit

import Data.List
import Data.Char
import Database.HDBC
import Database.HDBC.PostgreSQL

import Web.Mapper.DB.Meta
import Web.Mapper.DB.Sql
import Web.Mapper.Mapper
import TestGenerator

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

main = defaultMain [dbPrivilegeTest] 

dbPrivilegeTest = $testGroupGenerator

connectionString = "dbname=DbPrivilegeTest user=oscar password=oscar"

-- Test table rights
testOscarCanOnlySelect =
  do conn <- connectPostgreSQL connectionString
     types <- pgType conn
     let test_select = find (\t -> typeName t == "test_select") types
         actual = maybe [] tableInfoPrivilege test_select
         expected = [(SelectPrivilege,False)]
     expected @=? actual

testOscarCanOnlyInsert =
  do conn <- connectPostgreSQL connectionString
     types <- pgType conn
     let test_select = find (\t -> typeName t == "test_insert") types
         actual = maybe [] tableInfoPrivilege test_select
         expected = [(InsertPrivilege,False)]
     expected @=? actual

testOscarCanOnlyUpdate =
  do conn <- connectPostgreSQL connectionString
     types <- pgType conn
     let test_select = find (\t -> typeName t == "test_update") types
         actual = maybe [] tableInfoPrivilege test_select
         expected = [(UpdatePrivilege,False)]
     expected @=? actual

testOscarCanOnlyDelete =
  do conn <- connectPostgreSQL connectionString
     types <- pgType conn
     let test_select = find (\t -> typeName t == "test_delete") types
         actual = maybe [] tableInfoPrivilege test_select
         expected = [(DeletePrivilege,False)]
     expected @=? actual


