{ lib, ... }:

with lib;
with lib.my;
{
  options = with types; {
    users.primaryUser = {
      username = mkOpt str null;
      fullName = mkOpt str null;
      email = mkOpt str null;
      nixConfigDirectory = mkOpt str null;
    };
  };
}
