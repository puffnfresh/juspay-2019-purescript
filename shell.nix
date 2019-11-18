with import <nixpkgs> { };

runCommand "true" {
  buildInputs = [
    purescript
    nodePackages.bower
    psc-package
    nodePackages.pulp
  ];
} ""
