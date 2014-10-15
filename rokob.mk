.PHONY: all deps app test rel docs clean help

REBAR=./rebar
RELX=./relx

ROKOB_MK_VERSION = 1

V ?= 0

gen_verbose_0 = @echo " GEN   " $@;
gen_verbose = $(gen_verbose_$(V))

all:: deps app rel

clean::
	$(gen_verbose) rm -f erl_crash.dump

help::
	@printf "%s\n" \
		"rokob.mk (version $(ROKOB_MK_VERSION))" \
		"Copyright (c) 2014 rokob <wvvwwvw@gmail.com>" \
		"" \
		"Usage: [V=1] make [target]" \
		"" \
		"Targets:" \
		"  all		Run deps, app, and rel targets" \
		"  deps		Fetch dependencies (if needed) and compile them" \
		"  app		Compile the project" \
		"  rel		Generate a release" \
		"  test		Run eunit tests" \
		"  docs		Generate the documentation using edoc" \
		"  clean	        Clean up this mess" \
		"  help		Output this and exit" \
		""

deps::
	@$(REBAR) get-deps

compile::
	@$(REBAR) compile

clean::
	$(gen_verbose) rm -f erl_crash.dump
	@$(REBAR) clean

distclean:: clean
	@$(REBAR) delete-deps

test::
	@$(REBAR) eunit skip_deps=true

app::
	@$(REBAR) compile skip_deps=true

apistart:: app
	@exec erl -pa $(PWD)/apps/*/ebin -pa $(PWD)/deps/*/ebin \
	    -config $(PWD)/apps/*/priv/app.config \
	    -s bam_api

rel:: deps app
	@$(RELX)

docs::
	@$(REBAR) doc
