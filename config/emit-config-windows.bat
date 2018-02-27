set OS=%1
set CLUSTER=%2
set CONFIG=%3

set isMainnet=False
set isStaging=False
@      if %CLUSTER% equ mainnet ( set isMainnet=True
) else if %CLUSTER% equ staging ( set isStaging=True
) else (
  echo "FATAL: unsupported cluster value '%CLUSTER%.  Valid options: mainnet staging" & exit /b 1
)
set isWin64=False
set isMacos64=False
@      if %OS%      equ macos64 ( set isWin64=True
) else if %OS%      equ win64   ( set isMacos64=True
) else (
  echo "FATAL: unsupported OS value '%OS%.  Valid options: macos64 win64" & exit /b 1
)
set flags="{ isMainnet=%isMainnet%, isStaging=%isStaging%, isMacos64=%isMacos64%, isWin64=%isWin64% }"
@      if %CONFIG%  equ launcher ( set CONFIG=./launcher.dhall
) else if %CONFIG%  equ topology ( set CONFIG=./topology.dhall
) else (
  @echo "FATAL: unsupported config value '%CONFIG%.  Valid options: launcher topology" & exit /b 1
)

echo %CONFIG% %flags:"=% > toplevel.dhall


dhall < toplevel.dhall | dhall-to-yaml
