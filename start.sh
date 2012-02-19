erl +K true -name jsonpath@127.0.0.1 -pa ebin -pa deps/*/ebin \
	-config priv/app.config \
	-boot start_sasl -s reloader -s lager
