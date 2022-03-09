{ lib, ... }:

let
  inherit (builtins) attrValues readFile;
  inherit (lib) concatStringsSep filterAttrs fold isAttrs mapAttrs' mkOption types;
in rec {
  /* Recursively generates a list of values of ‹attr› even for nested attrs

     Type:
       attrValuesRec :: AttrSet -> [x]

     Example:
       attrValuesRec { foo = { bar = "baz"; }; a = "b"; }
       => ["baz" "b"]
  */
  attrValuesRec = attr: fold (x: xs: (if isAttrs x then attrValuesRec x else [x]) ++ xs) [] (attrValues attr);

  /* Filter the ‹self› key from the given ‹attr›

     Type:
       filterSelf :: AttrSet -> AttrSet
     
     Example:
       filterSelf { foo = "bar"; self = "baz"; }
       => { foo = "bar"; }
  */
  filterSelf = attr: filterAttrs (n: _: n != "self") attr;

  /* Maps the items of ‹list› to strings and concatenates them with ‹sep› in
     between the individual items

     Type:
       joinWithSep :: [a] -> String -> String
       ‹a› should be a type that is convertable to string using ‹toString›
     
     Example:
       joinWithSep [ 42 "foo" 0 ] "-"
       => "42-foo-0"
  */
  joinWithSep = list: sep: concatStringsSep sep (map toString list);

  /* Reads the given ‹path› and appends the ‹extras› to it

     Type:
       configWithExtras :: Path -> String -> String
     
     Example:
       configWithExtras example.txt "Appended text"
       => "Some text from example\nAppended text"
       Given that ‹example.txt› contains "Some text from example"
  */
  configWithExtras = path: extras: "${readFile path}\n${extras}";

  enable = { enable = true; };
}
