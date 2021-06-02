.PHONY: setup run

setup:
	cp defaultQuotes quotes
	cp app/config.example.json app/config.json
	echo "Legg til sitater i `quotes`"
	echo "Legg botens token og div. andre innstillinger i `app/config.json`"

run:
	stack run
