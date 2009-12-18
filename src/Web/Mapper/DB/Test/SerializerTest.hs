{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell #-}
module Web.Mapper.DB.Test.SerializerTest where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import Test.HUnit

import Data.List

import TestGenerator
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Web.Mapper.DB.Meta
import Web.Mapper.DB.Serializer
import Text.XML.Light
import Utilities.Misc

main = defaultMain [serializerTest] 

serializerTest = $testGroupGenerator

test_empty_serializer =
  do let i = DbInfo [] []
         actual = serialize i
         expected = "<api />"
     expected @=? actual

test_empty_json_serializer =
  do let i = DbInfo [] []
         actual = jsonSerialize i
         expected = "{\"api\": {\"types\": [], \"functions\": []}}"
     expected @=? actual

test_simple_json_serializer =
  do let procs = [ProcInfo boolPrimInfo "name" "public" [("fst", int4PrimInfo)] (Just "Coolness")]
         tables = [int4PrimInfo, TableInfo "Foo" "public" [ColumnInfo "col" int4PrimInfo] []]
         i = DbInfo tables procs
         actual = jsonSerialize i
         expected =  "{\"api\": \
         \ {\"types\": \
         \  [{\"name\": \"Foo\", \"ns\": \"public\", \"columns\": \
         \      [{\"label\": \"col\", \"name\": \"int4\", \"ns\": \"pg_catalog\"}]}],\
         \  \"functions\": [ \
         \    { \"name\": \"name\", \"ns\": \"public\",\
         \      \"returntype\": {\"name\": \"bool\", \"ns\": \"pg_catalog\"}, \
         \      \"comment\": \"Coolness\" , \
         \      \"params\": [{\"label\": \"fst\", \"name\": \"int4\", \"ns\": \"pg_catalog\"}] \
         \ }]}}"
     (removeSpace expected) @=? (removeBreak $ removeSpace actual)
