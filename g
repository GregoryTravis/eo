ghc -o eo eo.hs
echo ok
receivemidi dev "US-122 MKII MIDI" | ./eo | sendmidi dev "US-122 MKII MIDI" --
#./eo
