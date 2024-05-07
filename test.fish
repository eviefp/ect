#!/usr/bin/env fish

while true;
      inotifywait -e modify src/**.hs test/**.hs;
      clear;
      cabal run ect-test --
end;
