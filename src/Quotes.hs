{-# LANGUAGE OverloadedStrings #-}
module Quotes where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T

type Quote = (Text, Text)

formatQuote :: Quote -> Text
formatQuote (text, citation)
  | text == ""     = "Fant ingen sitater! Legg til med `!nyttSitat`."
  | citation == "" = T.concat ["> ", text]
  | otherwise      = T.concat ["> ", text, "\n- ", citation]

readQuote :: Text -> Quote
readQuote str = (text, citation)
 where
  idx      = T.findIndex (== ';') str
  text     = T.strip $ maybe str (\i -> fst $ T.splitAt i str) idx
  citation = T.strip $ maybe "" (\i -> snd $ T.splitAt (i + 1) str) idx

donnJokes :: [Quote]
donnJokes =
  [ ("In our opinion we've **donn** quite a fantastic job", "Sir T'Homst, 2021")
  , ("Jeg skal svare når jeg er **DONN** med å lese gjennom de", "Helene, 2021")
  , ("Jeg har lang erfaring med **Donn**-spøker", "Helene, 2021")
  , ("Is there something you **Donn't** feel like is relevant?", "Helene, 2021")
  , ("A wild **Donn** appeared"                 , "Tore, 2021")
  , ("Da tror jeg vi er **Donn**"               , "Tore, 2021")
  , ("Jeg gleder meg til å være **Donn** med wireframene", "Helene, 2021")
  , ("(...) og så er vi vel **Donn**"           , "Helene, 2021")
  , ("**Donn't** want to do that"               , "Helene, 2021")
  , ("dun dun **donn**"                         , "Tore, 2021")
  , ("We've <:donn:836899970330001408> it"      , "Helene, 2021")
  ]
