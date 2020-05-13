######
#
# Author: Brandon Barker
#
######

with import <nixpkgs> { };
let
  # unstable = import (fetchTarball https://nixos.org/channels/nixos-unstable/nixexprs.tar.xz) { };
  ACTIVE_CAC_PROJECTS="CarpentryMain CarpentryFine Tutorials ProjectA ProjectB";
in
stdenv.mkDerivation {
  name = "hledger-shell";
  buildInputs = [
    hledger hledger-ui hledger-web
    pcre.out
    zlib
    stack
  ];
  shellHook = ''
    export LD_LIBRARY_PATH=${pcre.out}/lib:${zlib}/lib:$LD_LIBRARY_PATH
    export LEDGER_FILE=$(pwd)/timekeeping.journal
    export ACTIVE_CAC_PROJECTS="${ACTIVE_CAC_PROJECTS}"
    export ACTIVE_CAC_ENTRIES="${ACTIVE_CAC_PROJECTS} Reading Tickets Misc Vacation Sick"
  '';  
}
