@day DAY:
	if ! test -f input/day{{DAY}}.in; then echo "Fetching input file..." && python3 scripts/fetch_input.py {{DAY}}; fi
	cd src && runhaskell day{{DAY}}.hs
