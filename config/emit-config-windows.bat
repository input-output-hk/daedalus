set OS=%1
set CLUSTER=%2
set CONFIG=%3

@      if %CLUSTER% eq mainnet ( @true
) else if %CLUSTER% eq staging ( @true
) else (
  echo "FATAL: unsupported cluster value '%CLUSTER%.  Valid options: mainnet staging" & exit /b 1
)
@      if %OS%      eq macos64 ( @true
) else if %OS%      eq win64   ( @true
) else (
  echo "FATAL: unsupported OS value '%OS%.  Valid options: macos64 win64" & exit /b 1
)
@      if %CONFIG%  eq launcher (
  set COMPONENTS="./common.dhall // ./%CLUSTER%.dhall // ./%OS%.dhall // ./%CLUSTER%-%OS%.dhall"
) else if %CONFIG%  eq wallet-topology (
  set COMPONENTS="./topology-%CLUSTER%.dhall"
) else (
  @echo "FATAL: unsupported config value '%CONFIG%.  Valid options: launcher wallet-topology" & exit /b 1
)

rem XXX: too much pain for now
rem nix-shell --show-trace -p dhall-json dhall \
rem           --run \
rem           "bash -c \"echo \\\"${components}\\\" | dhall | dhall-to-yaml\""
