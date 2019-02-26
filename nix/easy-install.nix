{ pkgs ? (import <nixpkgs> {}) }:
{
  ntangle = pkgs.callPackage ./ntangle.nix {};
}
