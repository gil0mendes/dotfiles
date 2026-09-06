self: super: {
  myKanata = self.callPackage ./packages/kanata.nix { };

  folly = super.folly.overrideAttrs (old: {
    patches = old.patches ++ [ ./packages/folly-memset-benchmark-darwin.patch ];
    disabledTests =
      old.disabledTests
      ++ self.lib.optionals (self.stdenv.hostPlatform.isDarwin && self.stdenv.hostPlatform.isx86_64) [
        "thread_local_test.ThreadLocal.Fork"
      ];
  });
}
