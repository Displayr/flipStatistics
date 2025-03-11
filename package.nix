{ pkgs ? import <nixpkgs> {}, displayrUtils }:

pkgs.rPackages.buildRPackage {
  name = "flipStatistics";
  version = displayrUtils.extractRVersion (builtins.readFile ./DESCRIPTION); 
  src = ./.;
  description = ''
    Computes standard statistics, dealing with situations not addressed
    in base R. E.g., weighting, non-standard data structures.
  '';
  propagatedBuildInputs = with pkgs.rPackages; [ 
    verbs
    rhtmlHeatmap
    flipU
    flipTransformations
    survey
    flipFormat
  ];
}
