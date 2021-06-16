{ pkgs ? import <nixos> {}, ... }:
# TODO: pin nixpkgs
pkgs.stdenv.mkDerivation {
  name = "url-shortener";
  buildInputs = with pkgs; [ zlib postgresql_12 ];
}
