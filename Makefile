all:
	./run-omake.sh

test:
	./run-omake.sh test

test-gui:
	./run-omake.sh test-gui

clean:
	omake clean
