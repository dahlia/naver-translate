{-# LANGUAGE OverloadedStrings #-}
module Language.Translate.Naver ( translate
                                , translateMultiple
                                , translateUrl
                                ) where

import Control.Lens ((^?))
import Control.Monad (liftM)
import Data.Aeson.Lens (key)
import Data.Aeson.Types (Value(String))
import Data.LanguageCodes (ISO639_1(JA, KO), language)
import Data.Text (Text, cons, intercalate, pack, snoc, splitOn, strip)
import Network.URI (URI(URI), URIAuth(URIAuth), relativeTo, uriIsAbsolute)
import Network.Wreq (FormParam((:=)), post, responseBody)
import System.Random (getStdRandom, randomR)

-- | Translate a string.
translate :: ISO639_1 -- ^ The language translated from
          -> ISO639_1 -- ^ The language to translate to
          -> Text     -- ^ The text to translate
          -> IO Text  -- ^ The translated text
translate sourceLanguage targetLanguage text =
    let response =
            post
            "http://translate.naver.com/translate.dic"
            [ "query" := text
            , "srcLang" := pack (language sourceLanguage)
            , "tarLang" := pack (language targetLanguage)
            , "highlight" := ("0" :: Text)
            , "hurigana" := ("0" :: Text)]
        getResultData resp = resp ^? responseBody . key "resultData"
        resultData = liftM getResultData response
    in
        resultData >>= \dat -> case dat of
            Just (String translated) -> return translated
            _ -> ioError $ userError "translate.naver.com sent invalid response"

arbitraryNumericLength :: Int
arbitraryNumericLength = 20

arbitraryNumeric :: IO Text
arbitraryNumeric =
    let
        randomDigit i = getStdRandom (randomR (if i > 1 then '0' else '1', '9'))
        string = mapM randomDigit [1..arbitraryNumericLength]
    in
        liftM pack string

-- | Translate multiple strings at one go.
translateMultiple :: ISO639_1  -- ^ The language translated from
                  -> ISO639_1  -- ^ The language to translate to
                  -> [Text]    -- ^ The texts to translate
                  -> IO [Text] -- ^ The translated texts
translateMultiple sourceLanguage targetLanguage texts = do
    spliter <- arbitraryNumeric
    let bundle = intercalate (snoc (cons ' ' spliter) ' ') texts
    result <- translate sourceLanguage targetLanguage bundle
    return $ map strip $ splitOn spliter result

-- | Get the url of the translated page.  Other languages than Korean
-- ('KO') and Japanese ('JA') are unsupported, so they get 'Nothing'.
translateUrl :: ISO639_1  -- ^ The language translated from
             -> ISO639_1  -- ^ The language to translate to
             -> URI       -- ^ The url of the page to translate
             -> Maybe URI -- ^ The url of the translated page
translateUrl sourceLanguage targetLanguage (URI scheme authority path query _) =
    case (scheme, authority) of
        ("http:", Just (URIAuth "" host port)) ->
            let relUrl = getRelUrl host port
                getUrl = Just . relativeTo relUrl
            in case langPair of
                (JA, KO) -> getUrl j2k
                (KO, JA) -> getUrl k2j
                _ -> Nothing
        _ -> Nothing
  where
    langPair = (sourceLanguage, targetLanguage)
    baseAuth = Just $ URIAuth "" "jptrans.naver.net" ""
    j2k = URI "http:" baseAuth "/j2k_frame.php/korean/" "" ""
    k2j = URI "http:" baseAuth "/webtrans.php/korean/" "" ""
    getRelUrl host port = URI "" Nothing (host ++ port ++ path) query ""
