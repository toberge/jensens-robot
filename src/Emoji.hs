{-# LANGUAGE OverloadedStrings #-}

module Emoji where

import           Data.Text                      ( Text )

-- TODO Textify & use emojis from the actual server

-- | Alternate error thing
err2 = "<:dumpster:839475235530080338>"

-- | For error messages
err = "<:lejosstor:692329957162090577>"

-- | Alternatives for error messages
errs :: [Text]
errs =
  [ "<:lejos:692328492972638238>"
  , "<:lejosstor:692329957162090577>"
  , "<:dumpster:839475235530080338>"
  , "<:brumm:806467395694231604>"
  ]

-- | Substitute for cat photos
cats :: [Text]
cats =
  [ "<:kat:821343254225485853>"
  , "<:bigcat:831866342164529193>"
  , "<:impostor:839474040863195166>"
  ]

done = "<:donn:836899970330001408>"

nope = "<:bonk:804639608415191041>"

bonk = "<:bonk:804639608415191041>"

-- | Lisp functions

plus = "+"
minus = "-"
times = "*"
divide = "/"
power = "^"

equal = "<:round:707185921379795025>"
greater = "<:gr:814410373724897281>"
less = "<:hamstar:717345501682466852>"
greaterOrEqual = "<:gr:814410373724897281><:round:707185921379795025>"
lessOrEqual = "<:hamstar:717345501682466852><:round:707185921379795025>"
notEqual = "<:bonk:804639608415191041><:round:707185921379795025>"

list = "<:binwok:758966442350870560>"
size = "<:bigcat:831866342164529193>"
range = "<:spillsyltetoy:718072970332995665>"
reverse = "<:kat:821343254225485853>"
member = "<:eg:803547525415567411>"

whatIf = "<:brumm:806467395694231604>"
