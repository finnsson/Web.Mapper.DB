import qualified Hack
import qualified Hack.Handler.SimpleServer as Handler

import qualified Data.ByteString.Lazy.UTF8 as BSLU
import qualified Web.Mapper.MapperRestful as R
import Web.Mapper.Mapper
import Web.Mapper.DB.Meta
import Web.Mapper.DB.RuntimeDbMapper
import Web.Mapper.MapperSerializer

main :: IO ()
main = Handler.run 3011 app

app :: Hack.Env -> IO Hack.Response
app env = do
    dbOutput <- getViewMap "dbname=RuntimeDbMapperTest user=test password=test" dataInput
    return $ Hack.Response
        200
        [("Content-Type", "text/plain; charset=utf-8")]
        $ BSLU.fromString $ instruction ++ "\n\r\n\r" ++ (show dataInput) ++ "\n\r\n\r"
          ++ (serializeToXml $ dbOutput) ++ "\n\n\n" ++ (show env)
    where dataInput = getDataInput $ R.envParser config env

config = R.EnvParser ["public"] ["func"] "_"

getDataInput (MapperInputData v) = v
getDataInput _ = DataInput Create False "xml" "apa" "apa" [] []

instruction = "Test the query http://localhost:3011/public/int_int&fst=\"11\"/"