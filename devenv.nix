{ pkgs, ... }:
let
  browsers = (builtins.fromJSON (builtins.readFile "${pkgs.playwright-driver}/browsers.json")).browsers;
  chromium-rev = (builtins.head (builtins.filter (browser: browser.name == "chromium") browsers)).revision;
in
{
  env = {
    PLAYWRIGHT_BROWSERS_PATH = "${pkgs.playwright-driver.browsers}";
    PLAYWRIGHT_SKIP_VALIDATE_HOST_REQUIREMENTS = true;
    PLAYWRIGHT_NODEJS_PATH = "${pkgs.nodejs}/bin/node";
    PLAYWRIGHT_LAUNCH_OPTIONS_EXECUTABLE_PATH = "${pkgs.playwright-driver.browsers}/chromium-${chromium-rev}/chrome-linux64/chrome";
  };

  packages = with pkgs; [
    haskellPackages.fourmolu
    haskellPackages.hspec-golden
    playwright-driver.browsers
  ];
  languages.haskell = {
    enable = true;
    package = pkgs.haskell.compiler.ghc912;
  };
  languages.javascript = {
    enable = true;
    npm.enable = true;
  };
}
