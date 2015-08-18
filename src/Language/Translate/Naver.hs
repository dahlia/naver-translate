{-# LANGUAGE OverloadedStrings #-}
module Language.Translate.Naver ( translate
                                , translateMultiple
                                ) where

import Control.Lens ((^?))
import Control.Monad (liftM)
import Data.Aeson.Lens (key)
import Data.Aeson.Types (Value(String))
import Data.LanguageCodes (ISO639_1, language)
import Data.Text (Text, cons, intercalate, pack, snoc, splitOn, strip)
import Network.Wreq (FormParam((:=)), post, responseBody)
import System.Random (getStdRandom, randomR)

translate :: ISO639_1 -> ISO639_1 -> Text -> IO Text
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

translateMultiple :: ISO639_1 -> ISO639_1 -> [Text] -> IO [Text]
translateMultiple sourceLanguage targetLanguage texts = do
    spliter <- arbitraryNumeric
    let bundle = intercalate (snoc (cons ' ' spliter) ' ') texts
    result <- translate sourceLanguage targetLanguage bundle
    return $ map strip $ splitOn spliter result
