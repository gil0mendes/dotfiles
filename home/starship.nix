{ pkgs, ... }:

{
  # Starship Prompt
  # https://rycee.gitlab.io/home-manager/options.html#opt-programs.starship.enable
  programs.starship.enable = true;

  programs.starship.settings = {
    # See docs here: https://starship.rs/config/
    # Symbols config configured ./starship-symbols.nix.

    format = "$directory\${custom.scm}$all";

    character = {
	    error_symbol = "[󰘧](bold red)";
	    success_symbol = "[󰘧](bold green)";
    };

    git_branch = {
    	disabled = true;
    };

    git_status = {
    	disabled = true;
    };

    git_commit = {
	    disabled = true;
    };

	  custom.scm = {
	    when = "jj-starship detect";
	    shell = ["jj-starship" "--strip-bookmark-prefix" "gil0mendes/" "--truncate-name" "20" "--bookmarks-display-limit" "1"];
			format = "$output ";
		};

    # FIXME: the package is outdated, and the batery prop is no longer a map
    # battery.display.threshold = 25; # display battery information if charge is <= 25%
    directory.fish_style_pwd_dir_length = 1; # turn on fish directory truncation
    directory.truncation_length = 2; # number of directories not to truncate
    gcloud.disabled = true; # annoying to always have on
    hostname.style = "bold green"; # don't like the default
    memory_usage.disabled = true; # because it includes cached memory it's reported as full a lot
    username.style_user = "bold blue"; # don't like the default
  };

  home.packages = with pkgs; [
    jj-starship
  ];
}
