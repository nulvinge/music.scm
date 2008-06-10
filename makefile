all:	clear play
clear:
	clear
runmit: music.scm
	scheme --load music.scm </dev/null
run: music.scm
	gsi music.scm
debug:
	scheme --load music.scm --eval "(debug)"
play:	run
	timidity -Oe m.mid
playg:
	timidity -Oe m.mid -g 0.05

ndf: ndf.cpp
	g++ ndf.cpp -O2 -o ndf
runndf: ndf
	./ndf > ndfdata
plotndf: runndf
	gnuplot "ndfplot"
