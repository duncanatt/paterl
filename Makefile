################################################################################
## Project configurations                                                     ##
################################################################################

BIN=ebin
INCLUDE=include
SRC=src
TEST=test

# If the first argument is the target name 'run', strip the first word and keep
# the rest as command line arguments.
ifeq (run,$(firstword $(MAKECMDGOALS)))
  RUN_ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
  # Turn the remaining words as do-nothing targets to suppress Make warnings.
  $(eval $(RUN_ARGS):;@:)
endif

# Set shell to bash to use certain commands such as source.
SHELL=/bin/bash

define recursive
	$(shell find $(1) -name "*.$(2)")
endef

all: compile

compile: clean
	mkdir -p $(BIN)
	erlc -pa $(BIN) +debug_info -I $(INCLUDE) -W0 -o $(BIN) $(call recursive,$(SRC),erl)

compile-test: clean
	mkdir -p $(BIN)
	erlc -DTEST -Dlog -pa $(BIN) +debug_info -W0 -I $(INCLUDE) -W0 -o $(BIN) $(call recursive,$(SRC),erl)
	erlc -DTEST -Dlog -pa $(BIN) -I $(INCLUDE) -W0 -o $(BIN) $(call recursive,$(TEST),erl)

test: compile-test
	erl -noshell -pa $(BIN) -eval 'case eunit:test(log_tracer_test, [verbose]) of error -> init:stop(1); Result -> Result end.' -s init stop
	erl -noshell -pa $(BIN) -eval 'case eunit:test(async_tracer_test, [verbose]) of error -> init:stop(1); Result -> Result end.' -s init stop

run: compile
	@echo "prog $(RUN_ARGS)"
	erl -noshell -pa $(BIN) -eval 'paterl:compile("$(RUN_ARGS)", [{includes,["include"]}])' -s init stop

clean:
	rm -rf $(BIN)/*.beam $(BIN)/*.E $(BIN)/*.tmp erl_crash.dump $(BIN)/*.app
