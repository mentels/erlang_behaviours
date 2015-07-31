.PHONY: test compile

compile:
	./rebar compile

test: logs compile
	ct_run -dir test -pa ebin -logdir logs

logs:
	mkdir logs
