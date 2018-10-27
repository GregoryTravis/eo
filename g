rm -f eo eo.hi
ghc -fprof-auto -fprof-cafs -prof -o eo eo.hs
echo ok
receivemidi dev "US-122 MKII MIDI" | ./eo +RTS -xc | sendmidi dev "US-122 MKII MIDI" --
#./eo
