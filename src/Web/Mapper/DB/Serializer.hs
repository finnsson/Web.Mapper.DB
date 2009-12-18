module Web.Mapper.DB.Serializer where

import Web.Mapper.DB.Meta
import Text.XML.Light
import Text.JSON
import Text.JSON.Types
import Text.JSON.Pretty
import Maybe

serialize :: DbInfo -> String
serialize (DbInfo typeInfos procInfos) =
  ppElement $ element "api" $ map serializeTypeInfo typeInfos ++ map serializeProcInfo procInfos

serializeTypeInfo ti =
  Elem $ element "test" []


serializeProcInfo pi =
  Elem $ element (procInfoName pi) []


-- helper functions
mkName n = QName n Nothing Nothing

element name content = Element (mkName name) [] content Nothing


jsonSerialize :: DbInfo -> String
jsonSerialize(DbInfo typeInfos procInfos) = show $ pp_value api
  where tables = mapMaybe serializeTableInfo typeInfos
        procs = map jsonSerializeProcInfo procInfos
        api = jHash "api" [ ("types", JSArray tables),("functions", JSArray procs) ]
 
serializeTableInfo :: TypeInfo -> Maybe JSValue
serializeTableInfo (TableInfo name ns cols priv) =
  Just $ jObject [
    ("name",jString name),
    ("ns", jString ns),
    ("columns", JSArray (map serializeColumn cols) )
  ]
serializeTableInfo _ = Nothing

serializeColumn (ColumnInfo name ti) = jObject [("label", jString name), ("name", jString $ typeName ti), ("ns", jString $ typeNs ti)]

jsonSerializeProcInfo :: ProcInfo -> JSValue
jsonSerializeProcInfo pi =
  jObject [
    ("name", jString $ procInfoName pi),
    ("ns", jString $ procInfoNS pi),
    ("returntype", jObject [("name", jString $ typeName returnType),("ns", jString $ typeNs returnType) ]),
    ("comment", maybe JSNull jString (procInfoComment pi)),
    ("params", JSArray (map serializeParams $ procInfoArguments pi ) )
  ] 
  where returnType = procInfoReturnType pi

serializeParams :: (String,TypeInfo) -> JSValue
serializeParams (name,ti) =
  jObject [("label", jString name),("name", jString $ typeName ti),("ns", jString $ typeNs ti)]

-- helper functions for JSON

jString = JSString . JSONString

jObject = JSObject . JSONObject

jHash n o = jObject [(n, jObject o)]

jArray n o = jObject [(n,JSArray o)]
