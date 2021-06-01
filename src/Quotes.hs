{-# LANGUAGE OverloadedStrings #-}
module Quotes where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T

type Quote = (Text, Text)

formatQuote :: Quote -> Text
formatQuote (text, citation) = T.concat ["> ", text, "\n- ", citation]

quotes :: [Quote]
quotes =
  [ ("Godt at datamaskinen gjør dette for oss", "Dag Olav, 2020")
  , ("Patch your shit", "Donn Morrison, 18.09.2020")
  , ("Gulrot"         , "Tore, 2021")
  , ("Gulrot"         , "Tore, 2021")
  , ( "Det fins alltid to måter å regne på: Min måte, og den enkle måten."
    , "Øyvind Solberg, 2021"
    )
  ]

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
