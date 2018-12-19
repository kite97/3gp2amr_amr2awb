all:
	rebar compile
	cd pfmc && $(MAKE)
clean:
	rebar clean
	cd pfmc && $(MAKE) clean
	rm -rf .eunit
test:
	rebar compile eunit

.PHONY: test
