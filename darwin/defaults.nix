{ config, ... }:

let
  userName = config.users.primaryUser.username;
in
{
  system.primaryUser = userName;

  system.defaults = {
    NSGlobalDomain = {
      AppleShowAllExtensions = true;
      "com.apple.trackpad.scaling" = 3.0;
      AppleInterfaceStyleSwitchesAutomatically = true;
      AppleMeasurementUnits = "Centimeters";
      AppleMetricUnits = 1;
      AppleShowScrollBars = "Automatic";
      AppleTemperatureUnit = "Celsius";
      InitialKeyRepeat = 15;
      KeyRepeat = 2;
      NSAutomaticCapitalizationEnabled = false;
      NSAutomaticDashSubstitutionEnabled = false;
      NSAutomaticPeriodSubstitutionEnabled = false;
    };

    CustomUserPreferences = {
      # disable Siri
      "com.apple.Siri" = {
        "UAProfileCheckingStatus" = 0;
        "siriEnabled" = 0;
      };

      # Disable personalized ads
      "com.apple.AdLib" = {
        allowApplePersonalizedAdvertising = false;
      };
    };

    # Show battery percentage in the menu bar
    controlcenter.BatteryShowPercentage = true;

    # Allow touch to click
    trackpad.Clicking = true;

    # Dock and Mission Control
    dock = {
      autohide = true;
      expose-group-apps = false;
      mru-spaces = false;
      tilesize = 128;
      # Disable all hot corners
      wvous-bl-corner = 1;
      wvous-br-corner = 1;
      wvous-tl-corner = 1;
      wvous-tr-corner = 1;
    };

    # Login and lock screen
    loginwindow = {
      GuestEnabled = false;
      DisableConsoleAccess = true;
    };

    # Spaces
    spaces.spans-displays = false;

    # Finder
    finder = {
      FXEnableExtensionChangeWarning = true;
    };
  };
}
