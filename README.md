# Jensens rørleggerrobot

## Oppsett

Denne boten krever
[Stack](https://docs.haskellstack.org/en/stable/README/)
og er beregna på å kjøre på et Unix-system.

For å sjekke status til en Minecraft-server trengs også Python-pakka
[mcstatus](https://github.com/Dinnerbone/mcstatus),
som kan installeres med `pip install mcstatus`.

Kjør `make setup` og følg instruksjonene for å sette opp serveren.

Kjør `stack build` for å bygge, `stack run` for å starte boten.

## Kommandoer

+ `!roll` for å kaste terning
+ `!cats` for å se kattebilder (wip), alias `!katt`
+ `!quote` for et sitat, alias `!sitat`
+ `!newQuote <sitat> ; <opphav>` for å foreslå et sitat, alias `!nyttSitat`
+ `!blame <noen>` for å legge skylda på noen (andre)
+ `!hug <noen>` for å gi noen en klem, alias `!klem`
  + `!blame` og `!hug` kan også kalles uten argumenter, da plukker de ut en tilfeldig person
+ `!suggest` for å foreslå en endring på serveren, alias `!foreslå`
+ `!mc` viser status for Minecraft-serveren
+ `!lisp <kode>` for å kjøre litt Lisp
+ `!lispHelp` hvis du ikke har den fjerneste anelse om hva Lisp er
