{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc865"
}: 

# let conduit-dbf = with nixpkgs; with stdenv; with haskellPackages;
let
  inherit (nixpkgs) haskellPackages;
  # conduit-dbf = import /home/pi/.config/nixpkgs/pkgs/conduit-dbf.nix {
  #   inherit (nixpkgs) stdenv zlib;
  #   inherit (haskellPackages) base binary bytestring conduit text
  #       conduit-combinators conduit-extra optparse-applicative mkDerivation; };
  isl = haskellPackages.callCabal2nix "isl" ./. {};
    
in import ../IHaskell/release.nix {
  inherit nixpkgs;
  inherit compiler;
  packages = haskellPackages: with haskellPackages; [
	  ihaskell-diagrams ihaskell-charts csv isl ];
}
