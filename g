rm -f eo eo.hi
ghc -fprof-auto -fprof-cafs -prof -o eo eo.hs
echo ok
(cat input ; cat) | (./eo +RTS -xc) 2>&1
#receivemidi dev "US-122 MKII MIDI" | ./eo +RTS -xc | sendmidi dev "US-122 MKII MIDI" --
#./eo
