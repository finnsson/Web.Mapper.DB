{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell #-}
{-|
  Thasql (Template Haskell SQL) autogenerates the Haskell code
  you need in order to call functions from a PostgreSQL database
  in a type safe manner.


  > $(connection "db=foo user=foo password=foo" "public")
-}
module Web.Mapper.DB.Meta where

import Database.HDBC
import Database.HDBC.PostgreSQL
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Char
import Data.Maybe
import List
import Control.Monad

import Utilities.Misc

import Web.Mapper.DB.Sql

import Web.Mapper.Mapper

-- DATA TYPES


type Id = Integer

type FuncName = String

type FuncNS = String

type TypeName = String

type TypeNS = String

type ArgName = String

type ArgPosition = Integer

type ColName = String

typeName :: TypeInfo -> TypeName
typeName (TableInfo tn _ _ _) = tn
typeName (PrimInfo tn _) = tn

typeNs :: TypeInfo -> TypeNS
typeNs (TableInfo _ tns _ _) = tns
typeNs (PrimInfo _ tns) = tns

tableInfoPrivilege (TableInfo _ _ _ p) = p
tableInfoPrivilege (PrimInfo _ _) = []

data Table = Table { tableDbType::DbType, tableDbColumns::[DbColumn] }
   deriving (Show,Eq)

data DbFunction = DbFunction {dbFunctionId::Integer, dbFunctionName::String, dbFunctionNS::String,dbFunctionTypeName::String, dbFunctionTypeNS::String,dbFunctionComment::Maybe String}
 deriving (Show, Eq)

data DbArgument = DbArgument {dbArgumentName::String, dbArgumentPosition::Integer, dbArgumentTypeName::String,dbArgumentTypeNS::String,dbArgumentFuncName::String,dbArgumentFuncNS::String}
 deriving (Show, Eq)

instance Ord DbArgument where
   compare l r = compare (dbArgumentPosition l) (dbArgumentPosition r)

data DbType = 
  DbType {
    dbTypeId::Id, 
    dbTypeTypeName::TypeName,
    dbTypeTypeNS:: TypeNS, 
    dbTypeComment:: String,
    dbTypePrivilege::[(PrivilegeType, Bool)]
  }
 deriving (Show, Eq)

data DbColumn = DbColumn {dbColumnName::String, dbColumnTypeName::String, dbColumnTypeNS::String, dbColumnId::Integer,dbColumnComment::Maybe String}
 deriving (Show, Eq)

-- DB CALL

sql2String = fromSql::(SqlValue->String)
sql2Integer = fromSql::(SqlValue->Integer)

picker:: [SqlValue] -> Int -> String
picker row col = sql2String $ row !! col
pickeri row col = sql2Integer $ row !! col
pickerb row col = (fromSql::(SqlValue->Bool)) $ row !! col

pickerMaybe :: [SqlValue] -> Int -> Maybe String
pickerMaybe row col =
  safePick (row !! col)
  where safePick SqlNull = Nothing
        safePick cell = Just $ sql2String cell

pgProc :: [TypeInfo] -> Connection -> IO [FuncInfo]
pgProc ti c = 
  do functions <- pgCall sqlFunctions sql2DbFunction c -- . getProcInfo
     arguments <- pgCall sqlArguments sql2DbArgument c -- . get
     return $ getProcInfos ti arguments functions

pgType :: Connection -> IO [TypeInfo]
pgType c =
  do types <- pgCall sqlTypes sql2DbType c
     columns <- pgCall sqlColumns sql2DbColumn c
     return $ getTypeInfo columns types 

pgCall :: String -> ([SqlValue] -> a) -> Connection -> IO [a]
pgCall pgSql pgTrans conn = quickQuery' conn pgSql [] >>* map pgTrans


-- SQL 2 a

sql2DbFunction :: [SqlValue] -> DbFunction
sql2DbFunction r = 
      let pick = picker r 
          picki = pickeri r
          id = picki 0
          funcName = pick 1
          funcNS = pick 2
          typeName = pick 3
          typeNS = pick 4
          comment = pickerMaybe r 5
      in DbFunction id funcName funcNS typeName typeNS comment  
          
sql2DbArgument :: [SqlValue] -> DbArgument
sql2DbArgument r =
   let pick = picker r
       picki =  pickeri r 
       funcNS = pick 0
       funcName = pick 1
       typeNS = pick 2
       typeName = pick 3
       argPosition = picki 4
       argName = pick 5
   in DbArgument argName argPosition typeName typeNS funcName funcNS 

sql2DbType :: [SqlValue] -> DbType
sql2DbType r =
   let pick = picker r
       picki = pickeri r
       pickb = pickerb r
       id = picki 0
       typeName = pick 1
       typeNS = pick 2
       comment = pick 3
       select = if pickb 4 then [(SelectPrivilege,False)] else []
       update = if pickb 5 then [(UpdatePrivilege,False)] else []
       insert = if pickb 6 then [(InsertPrivilege,False)] else []
       delete = if pickb 7 then [(DeletePrivilege,False)] else []
   in DbType id typeName typeNS comment $ select ++ update ++ insert ++ delete

sql2DbColumn :: [SqlValue] -> DbColumn
sql2DbColumn r =
   let pick = picker r
       picki = pickeri r
       id = picki 0 -- id of table
       colName = pick 1
       colType = pick 2
       colNS = pick 3
       comment = pickerMaybe r 4
   in DbColumn colName colType colNS id comment

-- get a Info

getTypeInfo :: [DbColumn] -> [DbType] -> [TypeInfo]
getTypeInfo columns dbTypes =
   let checkId dbType dbColumn = dbTypeId dbType == dbColumnId dbColumn
       tables = map (\t -> Table t $ filter (checkId t) columns) dbTypes
   in groupTableInfo tables

groupTableInfo :: [Table] -> [TypeInfo]
groupTableInfo table = 
   groupTableInfo' table primInfos
   where groupTableInfo' ta a | null ta = a
                              | otherwise =  groupTableInfo' ..% resolveTableGraph ta a

resolveTableGraph :: [Table] -> [TypeInfo] -> ([Table],[TypeInfo])
resolveTableGraph ta ti = (snd part,ti++ti')
  where sameFullName col typeInfo =  (typeNs typeInfo == dbColumnTypeNS col) && (typeName typeInfo == dbColumnTypeName col)
        onlyRefTypeInfo  = all (\col -> (any (sameFullName col) ti)) . tableDbColumns
        part = partition onlyRefTypeInfo ta
        ti' = map table2tableInfo $ fst part
          where columnInfo i = ColumnInfo (dbColumnName i) $ findTypeInfo ti (dbColumnTypeName i) (dbColumnTypeNS i)
                columnInfos = map columnInfo . tableDbColumns
                table2tableInfo r =  TableInfo (dbTypeTypeName dbt) (dbTypeTypeNS dbt) (columnInfos r) (dbTypePrivilege dbt) -- insert priv-info here
                  where dbt = tableDbType r 

getProcInfos :: [TypeInfo] -> [DbArgument] -> [DbFunction] -> [FuncInfo]
getProcInfos typeInfos dbArgs dbFunctions = map getProcInfo' dbFunctions
   where
      getProcInfo' dbFunc =
         ProcInfo {
            procInfoName = dbFunctionName dbFunc,
            procInfoReturnType = findTypeInfo typeInfos (dbFunctionTypeName dbFunc) (dbFunctionTypeNS dbFunc),
            procInfoComment = dbFunctionComment dbFunc,
            procInfoNS = dbFunctionNS dbFunc,
            procInfoArguments = map findArgType $ filter filterArg dbArgs
         }
         where 
            filterArg   a = dbArgumentFuncName a == dbFunctionName dbFunc && dbArgumentFuncNS a == dbFunctionNS dbFunc
            findArgType a = (dbArgumentName a, findTypeInfo typeInfos (dbArgumentTypeName a) (dbArgumentTypeNS a))

findTypeInfo :: [TypeInfo] -> String -> String -> TypeInfo
findTypeInfo ti tn nsn =
  fromMaybe stringPrimInfo (find (\t -> typeNs t == nsn && typeName t == tn) ti) 


-- All primary types in the PostgreSQL database.
primInfos = [stringPrimInfo,boolPrimInfo,int4PrimInfo, _int4PrimInfo, oidPrimInfo, bitPrimInfo]
stringPrimInfo = PrimInfo "String" "pg_catalog"
boolPrimInfo = PrimInfo "bool" "pg_catalog"
int4PrimInfo = PrimInfo "int4" "pg_catalog"
_int4PrimInfo = PrimInfo "_int4" "pg_catalog"
oidPrimInfo = PrimInfo "oid" "pg_catalog"
bitPrimInfo = PrimInfo "bit" "pg_catalog"

-- Export this function
dbInfo :: String -> IO MetaInfo
dbInfo connectionString =
   do conn <- connectPostgreSQL connectionString --"dbname=mydb user=foo password=bar"
      tableInfo <- pgType conn -- >>* groupTableInfo
      procInfo <- pgProc tableInfo conn
      let tableInfo' = map fromJust $ filter isJust $ map isTableInfo tableInfo
      return $ MetaInfo tableInfo' procInfo
   where
    isTableInfo (PrimInfo _ _) = Nothing
    isTableInfo t = Just t
