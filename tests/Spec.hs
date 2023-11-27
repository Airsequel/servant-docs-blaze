{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

import Protolude

import Protolude (
  Either (Left, Right),
  Eq,
  Maybe (Just, Nothing),
  Monoid (mempty),
  Text,
  not,
  IO, ($), pure,mempty,
  null,
  show,
  when,
  ($),
  (&),
  (.),
  (/=),
  (<&>),
  (<>),
 )
import Protolude qualified as P

import Data.Text qualified as T
import Data.Aeson qualified as Ae
import Data.Aeson.Encode.Pretty qualified as Ae
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BL
import Data.Monoid (mconcat)
import Data.String (String, fromString)
import Data.Text.Lazy.Encoding qualified as TLE
import Text.Blaze.Html5.Attributes qualified as A
import Text.Blaze.Internal (attribute)
import Text.RawString.QQ (r)
import Test.Hspec (hspec, shouldBe, describe, it)
import Servant.Server (Server)
import Servant.API ( (:>), PlainText, Get)
import Servant.Docs (ToSample(toSamples), singleSample)
import Servant.Docs (API, docs)
import Servant (Handler)
import Data.Text (Text, pack)
import Servant.API.ContentTypes (MimeRender(mimeRender))
import Data.Text.Encoding (encodeUtf8)
import Text.Blaze.Html.Renderer.Pretty (renderHtml)

import Servant.Docs.Blaze (apiToHtml)
import Servant.Docs.Blaze.Styling (cssContent)


type SimpleAPI = "words" :> Get '[PlainText] AppWord

newtype AppWord = AppWord Text
  deriving (Eq, Show)

instance ToSample AppWord where
  toSamples _ = singleSample $ AppWord "hello"

instance MimeRender PlainText AppWord where
  mimeRender _ (AppWord w) = w & encodeUtf8 & BL.fromStrict

simpleServer :: Server SimpleAPI
simpleServer =
  pure $ AppWord "hello"


simpleAPI :: Proxy SimpleAPI
simpleAPI = Proxy


apiDocs :: API
apiDocs = docs simpleAPI

expectedHtmlTxt :: Text
expectedHtmlTxt = [r|<!DOCTYPE HTML>

<html>
    <head>
        <title>
            JSON API Documentation
        </title>
        <style>
            {{css}}
        </style>
    </head>
    <body>
        <main>
            <h1>
                JSON API Documentation
            </h1>
            <div>
            </div>
            <hr>
            <div id="toc">
                <h2>
                    Table of Contents
                </h2>
                <ul>
                    <li class="endpoint" data-method="GET">
                        <a href="#GET-/words">
                            <span class="method-wrapper">
                                <span class="method">
                                    GET
                                </span>
                            </span>
                            <span>

                            </span>
                            <span class="path">
                                /words
                            </span>
                        </a>
                    </li>
                </ul>
            </div>
            <hr>
            <div>
                <h2 id="GET-/words" class="endpoint" data-method="GET">
                    <span class="method">
                        GET
                    </span>
                    <span>

                    </span>
                    <span class="path">
                        /words
                    </span>
                </h2>
                <p>

                </p>
                <div>
                </div>
                <div>
                    <h3>
                        Response
                    </h3>
                    <div>
                        <span>
                            text/plain;charset=utf-8
                        </span>
                    </div>
                    <h4>
                        Example Response Body
                    </h4>
                    <div>
                        <p>
                            text/plain;charset=utf-8
                        </p>
                        <pre>
                            hello
                        </pre>
                    </div>
                </div>
            </div>
        </main>
    </body>
</html>
|]


removeTrailingWhitespace :: String -> Text
removeTrailingWhitespace =
  T.unlines . fmap T.stripEnd . T.lines . T.pack



main :: IO ()
main = do
  hspec $ do
    describe "Servant Docs Blaze" $ do
      it "converts a Servant API to HTML" $ do
        let
          actualTxt :: Text = apiDocs
            & apiToHtml
            & renderHtml
            & removeTrailingWhitespace

          expectedTxt :: Text = expectedHtmlTxt
            & T.replace "{{css}}" (cssContent & T.pack)

        actualTxt `shouldBe` expectedTxt



