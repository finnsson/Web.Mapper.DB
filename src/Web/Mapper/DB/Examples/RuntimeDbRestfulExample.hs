import qualified Hack
import qualified Hack.Handler.SimpleServer as Handler

import qualified Data.ByteString.Lazy.UTF8 as BSLU
import qualified Web.Mapper.MapperRestful as R
import Web.Mapper.Mapper
import Web.Mapper.DB
import Web.Mapper.MapperSerializer

main :: IO ()
main = Handler.run 3011 app

app :: Hack.Env -> IO Hack.Response
app env = do
    dbOutput <- getMapperOutput runtimeDbMapper dataInput 
    return $ Hack.Response
        200
        [("Content-Type", "text/plain; charset=utf-8")]
        $ BSLU.fromString $ instruction ++ "\n\r\n\r" ++ (show dataInput) ++ "\n\r\n\r"
          ++ (serializer $ dbOutput) ++ "\n\n\n" ++ (show env)
    where
      dataInput = getDataInput $ R.envParser config env
      runtimeDbMapper = RuntimeDbMapper "dbname=RuntimeDbMapperTest user=test password=test" ["public"] []
      serializer = if (dataInputFormat dataInput == "xml") then serializeToXml else serializeToJson

config = R.EnvParser ["public"] ["func"] "_"

getDataInput (MapperInputData v) = v
getDataInput _ = DataInput Create False "xml" "apa" "apa" [] []

instruction = "Test the query http://localhost:3011/public/int_int&fst=\"11\"/"
