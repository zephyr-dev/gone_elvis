all: Main.js

Main.js: Main.hs
	hastec Main.hs # -main-is Main

clean:
	-rm -r main
	-rm *~
	-rm *.hi
	-rm *.o
	-rm Main.js

distclean: clean
	-rm Main
	-rm Main.js
