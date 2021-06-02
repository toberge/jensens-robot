.PHONY: setup run

setup:
	cp defaultQuotes quotes
	echo "Legg til sitater i `quotes`"
	echo "Legg botens token i `auth-token.secret`"

run:
	stack run
