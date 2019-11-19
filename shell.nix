with import <nixpkgs> { };

runCommand "true" {
  buildInputs = [
    purescript
    psc-package
    nodePackages.pulp
  ];
} ""
