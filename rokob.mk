.PHONY: all deps app test rel docs clean help start

REBAR=./rebar
RELX=./relx

ROKOB_MK_VERSION = 1

V ?= 0

gen_verbose_0 = @echo " GEN   " $@;
gen_verbose = $(gen_verbose_$(V))

all:: deps app rel

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
                "  start             Start up the app in the console" \
		"  help		Output this and exit" \
		""

deps::
	@$(REBAR) get-deps compile

compile::
	@$(REBAR) compile

clean::
	$(gen_verbose) rm -f erl_crash.dump
	@$(REBAR) clean

distclean:: clean
	@$(REBAR) delete-deps

test::
	@$(REBAR) eunit -r skip_deps=true

app::
	@$(REBAR) compile skip_deps=true

start:: app
	@exec ./_rel/bam/bin/bam console

rel:: deps app
	@$(RELX)

docs::
	@$(REBAR) -r skip_deps=true doc
