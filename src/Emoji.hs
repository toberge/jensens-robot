module Emoji where

import           Data.Text                      ( Text )

-- TODO Textify & use emojis from the actual server

-- | Alternate error thing
err2 = "<:dumpster:839475235530080338>" :: Text

-- | For error messages
err = "<:lejosstor:692329957162090577>" :: Text

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

done = "<:donn:836899970330001408>" :: Text
nope = "<:bonk:804639608415191041>" :: Text
bonk = "<:bonk:804639608415191041>" :: Text

brumm = "<:brumm:806467395694231604>" :: Text

gamerfargen = "<:gamerfargen:692675820812107806>" :: Text

minecraft = "<:minecraft:825679622002704414>" :: Text

-- | Lisp functions

plus = "+" :: Text
minus = "-" :: Text
times = "*" :: Text
divide = "/" :: Text
power = "^" :: Text

true = "<:gladeric:803249241953665034>" :: Text
false = "<:lejosstor:692329957162090577>" :: Text
null = "<:shrek:692318521354354728>" :: Text

equal = "<:round:707185921379795025>" :: Text
greater = "<:gr:814410373724897281>" :: Text
less = "<:hamstar:717345501682466852>" :: Text
greaterOrEqual = "<:gr:814410373724897281><:round:707185921379795025>" :: Text
lessOrEqual =
  "<:hamstar:717345501682466852><:round:707185921379795025>" :: Text
notEqual = "<:bonk:804639608415191041><:round:707185921379795025>" :: Text

list = "<:binwok:758966442350870560>" :: Text
size = "<:bigcat:831866342164529193>" :: Text
range = "<:spillsyltetoy:718072970332995665>" :: Text
reverse = "<:kat:821343254225485853>" :: Text
member = "<:eg:803547525415567411>" :: Text

lambda = "lambda" :: Text
whatIf = "<:brumm:806467395694231604>" :: Text
