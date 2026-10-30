# Installed products are distinct from the runtime environments they launch.
# Keep this contract pure so builders and lightweight checks can share it.
{
  mainnet = {
    profiles = ["mainnet"];
    payloadProfile = "mainnet";
    packageName = "daedalus-mainnet";
    displayName = "Daedalus Mainnet";
  };
  pilot = {
    profiles = ["preview" "preprod" "mainnet-flight"];
    payloadProfile = "mainnet-flight";
    packageName = "daedalus-pilot";
    displayName = "Daedalus Pilot";
  };
}
