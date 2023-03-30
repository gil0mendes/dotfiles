{ config, lib, pkgs, ... }:
{
  services.yabai = {
    enable = true;
    package = pkgs.yabai;
    enableScriptingAddition = true;
    config = {
      window_border = "on";
      window_border_width = 3;
      active_window_border_color = "0xff81a1c1";
      normal_window_border_color = "0xff3b4252";
      window_border_hidpi = "on";
      focus_follows_mouse = "autoraise";
      mouse_follows_focus = "off";
      mouse_drop_action = "stack";
      window_placement = "second_child";
      window_opacity = "off";
      window_topmost = "on";
      window_shadow = "float";
      window_origin_display = "focused";
      active_window_opacity = "1.0";
      normal_window_opacity = "1.0";
      split_ratio = "0.50";
      auto_balance = "on";
      mouse_modifier = "fn";
      mouse_action1 = "move";
      mouse_action2 = "resize";
      layout = "bsp";
      top_padding = 10;
      bottom_padding = 10;
      left_padding = 10;
      right_padding = 10;
      window_gap = 10;
    };

    extraConfig = ''
      # rules
      yabai -m rule --add app='System Preferences' manage=off
      yabai -m rule --add label="Firfox PIP" app="^Firefox$" title="^(Picture-in-Picture)$" manage=off
    '';
  };
}
