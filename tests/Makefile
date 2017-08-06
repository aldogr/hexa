.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<
	
.yrl.erl:
	erlc -W $<

.PHONY: edoc clean

ERL = erl -boot start_clean

MODS = pwmtest timingtest

all: compile

compile: ${MODS:%=%.beam}

clean:
	rm -rf *.beam erl_crash.dump edoc/

edoc:
	erl -noshell -eval "edoc:application(dsu_multicopter,\".\",[{dir, \"edoc/\"}])" -s init stop 
