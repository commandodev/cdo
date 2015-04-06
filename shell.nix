with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, aeson, base, bytestring, either, free, hedis, lens
             , pipes, pipes-concurrency, stdenv, tasty, tasty-hspec
             , tasty-quickcheck, text, transformers, uuid
             }:
             mkDerivation {
               pname = "cdo";
               version = "0.1.0.0";
               src = ./.;
               buildDepends = [
                 aeson base bytestring either free hedis lens pipes
                 pipes-concurrency text transformers uuid
               ];
               testDepends = [
                 base bytestring tasty tasty-hspec tasty-quickcheck
               ];
               homepage = "https://github.com/boothead/cdo";
               description = "Free Monad based CQRS";
               license = stdenv.lib.licenses.mit;
             }) {};
in
  pkg.env
