with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, aeson, attoparsec, base, bytestring, containers
             , either, errors, free, hedis, lens, mmorph, mtl, pipes
             , pipes-concurrency, stdenv, tasty, tasty-hspec, tasty-quickcheck
             , text, transformers, uuid
             }:
             mkDerivation {
               pname = "cdo";
               version = "0.1.0.0";
               src = ./.;
               buildDepends = [
                 aeson attoparsec base bytestring containers either errors free
                 hedis lens mmorph mtl pipes pipes-concurrency text transformers
                 uuid
               ];
               testDepends = [
                 base bytestring containers mmorph tasty tasty-hspec
                 tasty-quickcheck
               ];
               homepage = "https://github.com/boothead/cdo";
               description = "Free Monad based CQRS";
               license = stdenv.lib.licenses.mit;
             }) {};
in
  pkg.env
