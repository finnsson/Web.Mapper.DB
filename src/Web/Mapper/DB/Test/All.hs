{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell #-}
module Web.Mapper.DB.Test.All where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import Test.HUnit

-- import Data.List
-- import Data.Char
-- import Database.HDBC
-- import Database.HDBC.PostgreSQL

import Web.Mapper.DB.Sql

-- import Language.Haskell.TH
-- import Language.Haskell.TH.Syntax

import Web.Mapper.DB.Test.SqlTest (sqlTest)
import Web.Mapper.DB.Test.DbSimpleTest (dbSimpleTest)
import Web.Mapper.DB.Test.SerializerTest (serializerTest)
import Web.Mapper.DB.Test.DbPrivilegeTest (dbPrivilegeTest)

main = defaultMain groupsOfTests

-- replace the following line with TH that finds all *Test.hs in location-dir
-- and returns a list of all groupsOfTests in these source-files.
groupsOfTests = [sqlTest, dbSimpleTest , serializerTest , dbPrivilegeTest]
