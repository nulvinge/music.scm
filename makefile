all: doi

doi: clear run playmid

doc: clear runc playmid

dop: clear runp
dopc: clear compilep runpc

runp: midiparser.scm
	gsi midiparser.scm
compilep:
	gsc -link midiparser.scm
	gcc -o midiparser -Os midiparser.c midiparser_.c -lgambc -lm -ldl -lutil
runpc:
	./midiparser

clear:
	clear
runmit: music.scm
	scheme --load music.scm </dev/null
run: music.scm
	gsi music.scm
debug:
	scheme --load music.scm --eval "(debug)"
playmid:
	timidity -Oe m.mid
playg:
	timidity -Oe m.mid -g 0.05

compile_to_c: music.scm
	gsc -link music.scm systemfuncs.scm math.scm midi.scm
compile: compile_to_c
	gcc -o music -Os systemfuncs.c math.c midi.c midi_.c music.c -lgambc -lm -ldl -lutil
csize: compile
	ls -lh music
runc: csize
	./music

ndf: ndf.cpp
	g++ ndf.cpp -O2 -o ndf
runndf: ndf
	./ndf > ndfdata
plotndf: runndf
	gnuplot "ndfplot"
