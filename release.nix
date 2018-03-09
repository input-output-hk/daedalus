with import ./. {};
{
  inherit daedalus tarballInstaller;
  installerBundle = bundle.installerBundle;
  inherit newBundle;
}
