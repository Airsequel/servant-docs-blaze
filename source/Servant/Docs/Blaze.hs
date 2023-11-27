module Servant.Docs.Blaze (apiToHtml) where

import Protolude (
  Either (Left, Right),
  Eq,
  Maybe (Just, Nothing),
  Monoid (mempty),
  Text,
  not,
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

import Data.Aeson qualified as Ae
import Data.Aeson.Encode.Pretty qualified as Ae
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict qualified as HM
import Data.Monoid (mconcat)
import Data.String (String, fromString)
import Data.Text qualified as T
import Data.Text.Lazy.Encoding qualified as TLE
import Network.HTTP.Media (MediaType)
import Servant.Docs.Internal (
  API (_apiEndpoints, _apiIntros),
  Action (
    _authInfo,
    _captures,
    _fragment,
    _headers,
    _mxParams,
    _notes,
    _params,
    _response,
    _rqbody,
    _rqtypes
  ),
  DocCapture (_capDesc, _capSymbol),
  DocFragment (_fragDesc, _fragSymbol),
  DocIntro (_introBody, _introTitle),
  DocNote (_noteBody, _noteTitle),
  Endpoint (_method, _path),
  Response (_respBody, _respHeaders, _respStatus, _respTypes),
 )
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes (class_)
import Text.Blaze.Html5.Attributes qualified as A
import Text.Blaze.Internal (attribute)

import Servant.Docs.Blaze.Styling (cssContent)

dataAttr :: H.AttributeValue -> H.Attribute
dataAttr = attribute "data-method" " data-method=\""


endpointPathToStr :: [String] -> String
endpointPathToStr endpointPath =
  endpointPath
    & P.intersperse "/"
    & P.concat
    & ("/" <>)


normalizeEndpoints :: HM.HashMap Endpoint a -> [(Endpoint, a)]
normalizeEndpoints endpoints =
  endpoints
    & HM.toList
    & P.filter (\(endpoint, _action) -> endpoint._path /= [])
    & P.sortOn P.fst


getToc :: HM.HashMap Endpoint Action -> H.Html
getToc endpoints =
  H.div H.! A.id "toc" $ do
    H.h2 "Table of Contents"
    H.ul $
      endpoints
        & normalizeEndpoints
        <&> ( \(endpoint, _action) ->
                let
                  idSlug = getIdSlug endpoint
                  endpointStr = endpointPathToStr endpoint._path
                in
                  H.li
                    H.! class_ "endpoint"
                    H.! dataAttr
                      (fromString $ BSC.unpack endpoint._method)
                    $ do
                      H.a
                        H.! A.href (fromString $ "#" <> idSlug)
                        $ do
                          H.span H.! class_ "method-wrapper" $ do
                            H.span
                              H.! class_ "method"
                              $ fromString
                              $ BSC.unpack endpoint._method
                          H.span " "
                          H.span
                            H.! class_ "path"
                            $ fromString endpointStr
            )
        & mconcat


showHttpBody :: (Text, MediaType, BL.ByteString) -> H.Html
showHttpBody (text, mediaType, body) =
  H.div $ do
    when (text /= "") $
      H.p $
        fromString (show text)

    when
      (not $ "application/json" `T.isInfixOf` show mediaType)
      $ H.p (fromString (show mediaType))

    when (body /= "") $
      H.pre $
        fromString $
          T.unpack $
            case mediaType of
              "application/json" ->
                case Ae.eitherDecode body of
                  Left err -> "ERROR:" <> T.pack err
                  Right (val :: Ae.Value) ->
                    Ae.encodePretty'
                      Ae.defConfig{Ae.confIndent = Ae.Spaces 2}
                      val
                      & TLE.decodeUtf8
                      & P.toStrict
              "application/x-www-form-urlencoded" ->
                body
                  & BL.toStrict
                  & P.decodeUtf8
                  & T.replace "&" "&\n"
              _ ->
                body & BL.toStrict & P.decodeUtf8


{-| JSON is always included twice as "application/json;charset=utf-8"
and "application/json" (for whatever reason)
-}
filterDuplicate :: (P.Show a1) => (a2, a1, c) -> P.Bool
filterDuplicate (_, mediaType, _) =
  (show mediaType :: Text) /= "application/json;charset=utf-8"


getIdSlug :: Endpoint -> String
getIdSlug endpoint =
  (endpoint._method & BSC.unpack & fromString) <> "-"
   <> endpointPathToStr endpoint._path


apiToHtml :: API -> H.Html
apiToHtml api = do
  let
    renderIntros :: [DocIntro] -> H.Html
    renderIntros apiIntros = do
      let
        -- (primary, secondary)
        calloutTagToColors :: Text -> Maybe (String, String)
        calloutTagToColors tag =
          case tag & T.toLower of
            "note" -> Just ("hsl(216, 90%, 95%)", "hsl(216, 90%, 40%)")
            "tip" -> Just ("hsl(100, 90%, 95%)", "hsl(100, 90%, 40%)")
            "importat" -> Just ("hsl(30, 90%, 95%)", "hsl(30, 90%, 40%)")
            "warning" -> Just ("hsl(30, 90%, 95%)", "hsl(30, 90%, 40%)")
            "caution" -> Just ("hsl(0, 90%, 95%)", "hsl(0, 90%, 40%)")
            _ -> Nothing

        -- Returns (tag, title)
        splitTitle :: Text -> (Text, Text)
        splitTitle title =
          case title & T.splitOn "-" of
            (tag : calloutTitle) ->
              (tag & T.strip, calloutTitle & T.intercalate "-")
            _ -> ("", title)

      H.div $ do
        apiIntros
          & P.map
            ( \intro -> do
                let colors =
                      intro._introTitle
                        & T.pack
                        & splitTitle
                        & P.fst
                        & calloutTagToColors

                case colors of
                  Just (primary, secondary) ->
                    H.div
                      H.! class_ "callout"
                      H.! A.style
                        ( fromString $
                            ("background-color: " <> primary <> ";")
                              <> ("border-color: " <> secondary <> ";")
                        )
                      $ do
                        H.strong
                          H.! A.style ("color: " <> fromString secondary <> ";")
                          $ fromString intro._introTitle
                        intro._introBody
                          & P.map (H.p . H.preEscapedToHtml)
                          & mconcat
                  Nothing -> do
                    H.h2 $ fromString intro._introTitle
                    intro._introBody
                      & P.map (H.p . H.toHtml)
                      & mconcat
            )
          & mconcat

    renderEndpoint :: Endpoint -> H.Html
    renderEndpoint endpoint = do
      let
        methodStr = BSC.unpack endpoint._method
        idSlug = getIdSlug endpoint
        endpointStr = endpointPathToStr endpoint._path

      H.h2
        H.! A.id (fromString idSlug)
        H.! class_ "endpoint"
        H.! dataAttr (fromString methodStr)
        $ do
          H.span H.! class_ "method" $ fromString methodStr
          H.span " "
          H.span H.! class_ "path" $ fromString endpointStr

    notesTxt :: [DocNote] -> [Text]
    notesTxt =
      P.concatMap noteStr

    noteStr :: DocNote -> [Text]
    noteStr docNote =
      [ docNote._noteTitle & T.pack
      , ""
      , (docNote._noteBody <&> T.pack) & T.concat
      ]

    renderCaptures :: [DocCapture] -> H.Html
    renderCaptures captures = do
      let
        capturesTxt :: DocCapture -> H.Html
        capturesTxt docCapture =
          H.p $ do
            H.code $ fromString $ ":" <> docCapture._capSymbol
            H.span $ " - " <> fromString docCapture._capDesc

      H.div $
        captures
          <&> capturesTxt
          & mconcat

    showRespBody :: Action -> H.Html
    showRespBody action =
      action._response._respBody
        & P.filter filterDuplicate
        <&> showHttpBody
        & mconcat

    intercalate :: (Monoid a) => a -> [a] -> a
    intercalate sep =
      P.mconcat . P.intersperse sep

    renderMediaTypes :: [MediaType] -> H.Html
    renderMediaTypes rqtypes = do
      let
        rqtypesSpan (rqtype :: MediaType) =
          H.span $ fromString $ show rqtype

      H.div $
        rqtypes
          <&> rqtypesSpan
          & intercalate (H.span ", ")

    renderIfNotEmpty :: (P.Show a, Monoid a, Eq a) => a -> H.Html
    renderIfNotEmpty val
      | val P.== mempty = mempty
      | P.otherwise = H.p . H.toHtml . T.pack . P.show $ val

  H.docTypeHtml $ do
    H.head $ do
      H.title "JSON API Documentation"
      H.style $ fromString cssContent
    H.body $ do
      H.main $ do
        H.h1 "JSON API Documentation"
        renderIntros api._apiIntros
        H.hr
        getToc api._apiEndpoints
        H.hr
        H.div $ do
          api._apiEndpoints
            & normalizeEndpoints
            <&> ( \(endpoint :: Endpoint, action :: Action) -> do
                    renderEndpoint endpoint
                    H.p $ action._notes & notesTxt & T.concat & H.toHtml
                    renderIfNotEmpty action._authInfo
                    renderCaptures action._captures
                    renderIfNotEmpty action._headers
                    renderIfNotEmpty action._params
                    case action._fragment of
                      Just frag -> do
                        renderIfNotEmpty frag._fragSymbol
                        renderIfNotEmpty frag._fragDesc
                      Nothing -> mempty
                    renderIfNotEmpty action._notes
                    renderIfNotEmpty action._mxParams

                    when (action._rqbody /= []) $ do
                      H.h3 "Request"
                      renderMediaTypes action._rqtypes

                      H.h4 "Example Request Body"
                      action._rqbody
                        & P.filter filterDuplicate
                        <&> showHttpBody
                        & mconcat

                    H.div $ do
                      H.h3 "Response"
                      when
                        (action._response._respStatus /= 200)
                        (H.p $ fromString $ show action._response._respStatus)
                      renderMediaTypes action._response._respTypes
                      when
                        (not $ null action._response._respHeaders)
                        (H.p $ fromString $ show action._response._respHeaders)
                      H.h4 "Example Response Body"
                      showRespBody action
                )
            & mconcat

