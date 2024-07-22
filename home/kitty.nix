{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
  backgroundDependantColors = colors: with colors; {
    background = "#${base}";
    foreground = "#${main}";

    # Cursor
    cursor = "#${blue}";
    cursor_text_color = "#${base}";

    # Selection
    selection_background = "#${muted}";
    selection_foreground = "#${base}";

    # Tab bar
    tab_bar_background = "#${basehl}";
    inactive_tab_background = "#${strong}";
  };
in
{
  # Kitty terminal
  # https://sw.kovidgoyal.net/kitty/conf.html
  # https://rycee.gitlab.io/home-manager/options.html#opt-programs.kitty.enable
  programs.kitty.enable = true;

  # --- General config --- {{{
  programs.kitty.settings = {
    # https://fsd.it/shop/fonts/pragmatapro/
    # TODO: buy PragmataPro
    # font_family = "PragmataPro Mono Liga";
    font_family = "JetBrainsMono Nerd Font";
    # font_family = "Comic Code Ligatures";
    font_size = "14.0";
    adjust_line_height = "100%";
    disable_ligatures = "cursor"; # disable ligatures when cursor is on them

    # Window layout
    # hide_window_decorations = "titlebar-only";
    window_padding_width = "0";

    # Tab bar
    tab_bar_edge = "bottom";
    tab_bar_style = "powerline";
		tab_powerline_style = "slanted";
    tab_title_template = "{index}: {title}";
    active_tab_font_style = "bold";
    inactive_tab_font_style = "normal";
    tab_activity_symbol = "";  
  };

  # Keybindings
  programs.kitty.keybindings = {
    # Open new tab on the current directory
    "cmd+t" = "new_tab_with_cwd";

		# Close window
		"ctrl+backspace" = "close_window_with_confirmation ignore-shell";

		# Toggle layout
		"ctrl+'" = "toggle_layout stack";

		# Split window
		"ctrl+\\" = "launch --location=vsplit --cwd=current";
		"ctrl+enter" = "new_window_with_cwd";

		# Move window
		"shift+up" = "move_window up";
		"shift+left" = "move_window left";
		"shift+right" = "move_window right";
		"shift+down" = "move_window down";

		# Resize window
		"ctrl+=" = "resize_window taller 2";
		"ctrl+-" = "resize_window shorter 2";
		"ctrl+0" = "resize_window wider 2";
		"ctrl+9" = "resize_window narrower 2";
		"ctrl+8" = "resize_window reset";

		# Clear terminal - since we use ctrl-l to navigate
		"ctrl+;" = "clear_terminal scroll active";
  };

  # TODO: add support for italic fonts

  programs.kitty.extras.useSymbolsFromNerdFont = "JetBrainsMono Nerd Font";
  # }}} 

  # --- Colors config --- {{{
  programs.kitty.extras.colors = with pkgs.lib.colors; {
    enable = true;

    # Colors that aren't dependent on background
    common = with pkgs.lib.colors.catppuccin.colors; {
      # black
      color0 = "#${darkBasehl}";
      color8 = "#${darkBase}";
      # red
      color1 = "#${red}";
      color9 = "#${orange}";
      # green
      color2 = "#${green}";
      color10 = "#${darkestTone}";
      # yellow
      color3 = "#${yellow}";
      color11 = "#${darkTone}";
      # blue
      color4 = "#${blue}";
      color12 = "#${lightTone}";
      # magenta
      color5 = "#${magenta}";
      color13 = "#${violet}";
      # cyan
      color6 = "#${cyan}";
      color14 = "#${lightestTone}";
      # white
      color7 = "#${lightBasehl}";
      color15 = "#${lightBase}";
      # url underline color to fit colors
      url_color = "#${blue}";
      # tab bar
      active_tab_foreground = "#${lightBase}";
      active_tab_background = "#${green}";
      inactive_tab_foreground = "#${lightBase}";
    };

    # Background dependent colors
    dark = backgroundDependantColors catppuccin.dark;
    light = backgroundDependantColors catppuccin.light;
  };

  programs.fish.functions.set-term-colors = {
    body = "term-background $term_background";
    onVariable = "term_background";
  };
  programs.fish.interactiveShellInit = ''
    # Set term colors based on value of `$term_backdround` when shell starts up.
    set-term-colors
  '';
  # }}}
}
