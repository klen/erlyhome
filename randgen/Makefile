APP_NAME=randgen
VSN=0.1.0


all: compile

compile:
	erl -make

debug:
	erl +debug_info -o ebin src/*.erl

clean:
	rm -rf ebin/*.beam
	rm -rf doc/*

run: compile
	erl -pa ebin -noshell -s $(APP_NAME) start

shell: compile
	erl -pa ebin

docs:
	erl -noshell -run edoc_run application "'$(APP_NAME)'" \
	'"."' '[{def,{vsn,"$(VSN)"}}]'

appmon:
	erl -pa ebin -noshell -s appmon start

pman:
	erl -pa ebin -noshell -s pman start

webtool:
	erl -pa ebin -noshell -s webtool start

debugger:
	erl -pa ebin -noshell -s debugger start

tv:
	erl -pa ebin -noshell -s tv start

toolbar:
	erl -pa ebin -noshell -s toolbar start
