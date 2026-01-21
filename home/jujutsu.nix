{
  config,
  ...
}:
let
  isWork = config.home.user-info.username == "gmendes";
in
{
  programs.jujutsu = {
    enable = true;
    settings = {
      user = {
        name = config.home.user-info.fullName;
        email = config.home.user-info.email;
      };
      ui = {
        color = "auto";
        editor = "nvim";
        paginate = "auto";
      };
      signing = {
        behavior = "own";
        backend = "gpg";
      }
      // (
        if isWork then
          {
            key = "3900180E4467EA40BB5CC04411EC8E1407151F84";
          }
        else
          {
            key = "3C0FFA89D9EB7EF4BDABAB1B8108024DFE52031C";
          }
      );
      git = {
        signOnPush = true;
      };
    };
  };
}
