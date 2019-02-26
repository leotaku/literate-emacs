{ stdenv, fetchFromGitHub, callPackage }:
let
  nim = callPackage ./nim.nix {};
  cligen = fetchFromGitHub {
    owner = "c-blake";
    repo = "cligen";
    rev = "v0.9.19";
    sha256 = "1il2ldcz8fw951d2x8kv9d3hjxdz47gjqkw3cmd40a34f0kcxnzi";
  };
in
stdenv.mkDerivation rec {
  name = "ntangle-${version}";
  version = "49a941c";

  src = fetchFromGitHub {
    owner = "OrgTangle";
    repo = "ntangle";
    rev = "49a941c";
    sha256 = "04nc1bfpm8396xqkmqxpr8mbzyb4yg6zdjk8c4c95f6nv8msjy73";
  };

  buildInputs = [ nim ];

  buildPhase = ''
    cd src
    mkdir ../cache
    nim compile -p:${cligen} --nimcache:../cache ntangle.nim
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp ./ntangle $out/bin
  '';
}
