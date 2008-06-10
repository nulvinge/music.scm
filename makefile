all:	clear play
clear:
	clear
run: music.scm
	scheme --load music.scm </dev/null
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
