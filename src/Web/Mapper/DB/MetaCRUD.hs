-----------------------------------------------------------------------------
--
-- Module      :  Web.Mapper.DB.MetaCRUD
-- Copyright   :  
-- License     :  BSD4
--
-- Maintainer  :  Oscar Finnsson
-- Stability   :  
-- Portability :  
--
-- | Contains metaCRUD - a function that can work as a plugin for RuntimeDbMapper.
--
-----------------------------------------------------------------------------

module Web.Mapper.DB.MetaCRUD (
  metaCRUD,
  metaCreate,
  metaUpdate,
  metaRead,
  metaDelete
  ) where

import Web.Mapper.Mapper
import Web.Mapper.DB.RuntimeDbMapper
import Web.Mapper.DB.Meta
import Utilities.Misc
import Utilities.HDBC


import Database.HDBC
import Database.HDBC.PostgreSQL
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Char
import Data.Maybe
import List
import Control.Monad
import Maybe
import Data.Generics

-- | Works as a plugin for RuntimeDbMapper.
--
--   If the function is triggered by the content in DataInput it 
--   will return Right IO DataOutput,
--   otherwise Left DataInput.
metaCRUD :: RuntimeDbMapper -> DataInput -> Either DataInput (IO MapperOutput)
metaCRUD dbMapper dataInput = 
  if (dataInputMeta dataInput)
  then let
    cs = (runtimeDbMapperConnectionString dbMapper)
    meta = case (dataInputVerb dataInput) of
      Create -> metaCreate cs dataInput
      Read -> metaRead cs dataInput -- >>= (return . MapperOutputMeta)
      Update -> metaUpdate cs dataInput
      Delete -> metaDelete cs dataInput
    in Right meta
  else Left dataInput




  
joinError :: MapperOutput -> MapperOutput -> MapperOutput
joinError (MapperOutputError m) _ = MapperOutputError m
joinError _ (MapperOutputError m) = MapperOutputError m
joinError _ _ = MapperOutput []

type ColumnType = String -> String -> String -> String -> String 
type TableType = String -> String -> String

-- | Basic read-functionality. Reads everything at the moment.
metaRead :: String -> DataInput -> IO MapperOutput
metaRead cs di = do
  info <- dbInfo cs -- (runtimeDbMapperConnectionString rdm)
  return $ MapperOutputMeta info

metaX :: 
  TableType -> 
  ColumnType -> 
  String -> DataInput -> IO MapperOutput
metaX tableSql columnSql cs di =
  if cols == []
  then tableX tableSql cs di
  else mapM (columnX columnSql cs di) cols >>= (return . foldr joinError (MapperOutput []) )
    where cols = dataInputValue di 

tableX :: TableType -> String -> DataInput -> IO MapperOutput
tableX tableXsql cs di = do
  let name =  dataInputName di
      ns = dataInputNS di
  connection <- connectPostgreSQL cs
  quickQueryALsafe connection (tableXsql ns name) []

columnX :: ColumnType -> String -> DataInput -> (String,String) -> IO MapperOutput
columnX columnXsql cs di col = do
  let name =  dataInputName di
      ns = dataInputNS di
      columns = dataInputValue di
  connection <- connectPostgreSQL cs
  quickQueryALsafe connection (columnXsql ns name (fst col) (snd col)) []

-- | Create new tables and columns
metaCreate :: String -> DataInput -> IO MapperOutput
metaCreate = metaX createTableSql createColumnSql

-- | Delete tables and columns
metaDelete :: String -> DataInput -> IO MapperOutput
metaDelete = metaX deleteTableSql deleteColumnSql

-- \ Update tables and columns
metaUpdate = metaX updateTableSql updateColumnSql



-- SQL
createTableSql :: TableType
createTableSql ns name = "CREATE TABLE \"" ++ ns ++ "\".\"" ++ name ++ "\" () WITH ( OIDS = TRUE);"


deleteTableSql :: TableType
deleteTableSql ns name = "DROP TABLE \"" ++ ns ++ "\".\"" ++ name ++ "\";" 

createColumnSql :: ColumnType
createColumnSql ns name colName colType = "ALTER TABLE  \"" ++ ns ++ "\".\"" ++ name ++ 
  "\" ADD COLUMN \"" ++ colName ++ "\" " ++ colType ++ " ;" 

deleteColumnSql :: ColumnType
deleteColumnSql ns name colName colType = "ALTER TABLE  \"" ++ ns ++ "\".\"" ++ name ++ 
  "\" DROP COLUMN \"" ++ colName ++ "\";"

updateTableSql :: String -> String -> String
updateTableSql ns name = "" -- what should I do here???



updateColumnSql :: ColumnType
updateColumnSql ns name colName colType = "ALTER TABLE  \"" ++ ns ++ "\".\"" ++ name ++ 
  "\" ALTER COLUMN \"" ++ colName ++ "\" TYPE " ++ colType ++ ";"


-- Helpers
quickQueryALsafe :: Connection -> String -> [String] -> IO MapperOutput --[(String,String)]
quickQueryALsafe conn sql sqlParams = catchSql (withTransaction conn handleSuccess) handleError
  where
    sqlParams' = map SqlString sqlParams
    handleSuccess c = do
      res <- quickQueryAL c sql sqlParams'
      let res' = map (map (\i -> ( fst i ,show $ snd i))) res
      return $ MapperOutput res'
      
    handleError e = return $ MapperOutputError (show e)
