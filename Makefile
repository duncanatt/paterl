################################################################################
## Project configurations                                                     ##
################################################################################

BIN=ebin
INCLUDE=include
SRC=src
TEST=test
CMD=./src/paterl

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
	#erl -noshell -pa $(BIN) -eval 'case eunit:test(log_tracer_test, [verbose]) of error -> init:stop(1); Result -> Result end.' -s init stop
	#erl -noshell -pa $(BIN) -eval 'case eunit:test(async_tracer_test, [verbose]) of error -> init:stop(1); Result -> Result end.' -s init stop
	echo "CodeBEAM/ID server test"
	$(CMD) $(SRC)/examples/erlang/codebeam/id_server_demo.erl -v all -I include -O $(BIN)
	diff $(BIN)/id_server_demo $(TEST)/generated/codebeam/id_server_demo_ref
	echo "CodeBEAM/ID TS server test"
	$(CMD) $(SRC)/examples/erlang/codebeam/id_ts_server_demo.erl -v all -I include -O $(BIN)
	diff $(BIN)/id_ts_server_demo $(TEST)/generated/codebeam/id_ts_server_demo_ref
	echo "De'Liguoro & Padovani/Future test"
	$(CMD) $(SRC)/examples/erlang/de_liguoro_padovani/future.erl -v all -I include -O $(BIN)
	diff $(BIN)/future $(TEST)/generated/de_liguoro_padovani/future_ref
	echo "De'Liguoro & Padovani/Master-worker test"
	$(CMD) $(SRC)/examples/erlang/de_liguoro_padovani/master_worker.erl -v all -I include -O $(BIN)
	diff $(BIN)/master_worker $(TEST)/generated/de_liguoro_padovani/master_worker_ref
	echo "De'Liguoro & Padovani/Sessions test"
	$(CMD) $(SRC)/examples/erlang/de_liguoro_padovani/sessions.erl -v all -I include -O $(BIN)
	diff $(BIN)/sessions $(TEST)/generated/de_liguoro_padovani/sessions_ref
	echo "Savina/Cigarette smokers test"
	$(CMD) $(SRC)/examples/erlang/savina/cig_smok.erl -v all -I include -O $(BIN)
	diff $(BIN)/cig_smok $(TEST)/generated/savina/cig_smok_ref
	echo "Savina/Counter"
	$(CMD) $(SRC)/examples/erlang/savina/count.erl -v all -I include -O $(BIN)
	diff $(BIN)/count $(TEST)/generated/savina/count_ref
	echo "Savina/Fibonacci"
	$(CMD) $(SRC)/examples/erlang/savina/fib.erl -v all -I include -O $(BIN)
	diff $(BIN)/fib $(TEST)/generated/savina/fib_ref
	echo "Savina/Fibonacci (Pairs)"
	$(CMD) $(SRC)/examples/erlang/savina/fib_pairs.erl -v all -I include -O $(BIN)
	diff $(BIN)/fib_pairs $(TEST)/generated/savina/fib_pairs_ref
	echo "Savina/KFork"
	$(CMD) $(SRC)/examples/erlang/savina/kfork.erl -v all -I include -O $(BIN)
	diff $(BIN)/kfork $(TEST)/generated/savina/kfork_ref
	echo "Savina/Dining philosophers"
	$(CMD) $(SRC)/examples/erlang/savina/philosopher.erl -v all -I include -O $(BIN)
	diff $(BIN)/philosopher $(TEST)/generated/savina/philosopher_ref
	echo "Savina/Ping pong"
	$(CMD) $(SRC)/examples/erlang/savina/ping_pong.erl -v all -I include -O $(BIN)
	diff $(BIN)/ping_pong $(TEST)/generated/savina/ping_pong_ref
	echo "Savina/Ping pong (Strict)"
	$(CMD) $(SRC)/examples/erlang/savina/ping_pong_strict.erl -v all -I include -O $(BIN)
	diff $(BIN)/ping_pong_strict $(TEST)/generated/savina/ping_pong_strict_ref
	echo "Savina/Thread ring"
	$(CMD) $(SRC)/examples/erlang/savina/thread_ring.erl -v all -I include -O $(BIN)
	diff $(BIN)/thread_ring $(TEST)/generated/savina/thread_ring_ref


run: compile
	@echo "prog $(RUN_ARGS)"
	erl -noshell -pa $(BIN) -eval 'paterl:compile("$(RUN_ARGS)", [{includes,["include"]}])' -s init stop

clean:
	rm -rf $(BIN)/*.beam $(BIN)/*.E $(BIN)/*.tmp erl_crash.dump $(BIN)/*.app
