# Jensens rørleggerrobot

## Oppsett

Denne boten krever
[Stack](https://docs.haskellstack.org/en/stable/README/)
og er beregna på å kjøre på et Unix-system.

Kjør `make setup` og følg instruksjonene.

Kjør `stack build` for å bygge, `stack run` for å starte boten.

## Kommandoer

+ `!echo <whatever>` sier det du vil tilbake, alias `!ekko`
+ `!roll` for å kaste terning
+ `!cats` for å se kattebilder (wip), alias `!katt`
+ `!quote` for et sitat, alias `!sitat`
+ `!newQuote <sitat> ; <opphav>` for å foreslå et sitat, alias `!nyttSitat`
+ `!blame` for å legge skylda på noen andre
+ `!suggest` for å foreslå en endring på serveren, alias `!foreslå`
+ `!lisp <kode>` for å kjøre litt Lisp
+ `!lispHelp` hvis du ikke har den fjerneste anelse om hva Lisp er
