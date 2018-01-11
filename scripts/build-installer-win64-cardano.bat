@echo Obtaining Cardano from branch %CARDANO_BRANCH%
rmdir /s/q node_modules\daedalus-client-api 2>nul
mkdir      node_modules\daedalus-client-api

pushd node_modules\daedalus-client-api
  del /f CardanoSL.zip 2>nul
  ..\..\curl --location %CARDANO_URL% -o CardanoSL.zip
  if %errorlevel% neq 0 (
    @echo FAILED: couldn't obtain the cardano-sl package
    popd & exit /b 1
  )
  for /F "usebackq" %%A in ('CardanoSL.zip') do set size=%%~zA
  echo size is %size%
  if %size% lss %MIN_CARDANO_BYTES% (
    @echo FAILED: CardanoSL.zip is too small: threshold=%MIN_CARDANO_BYTES%, actual=%size% bytes
    popd & exit /b 1
  )

  7z x CardanoSL.zip -y
  @if %errorlevel% neq 0 (
    @echo FAILED: 7z x CardanoSL.zip -y
    popd & exit /b 1
  )
  del CardanoSL.zip
popd
@echo cardano-sl build-id:
type node_modules\daedalus-client-api\build-id
@echo cardano-sl commit-id:
type node_modules\daedalus-client-api\commit-id
@echo cardano-sl ci-url:
type node_modules\daedalus-client-api\ci-url

move   node_modules\daedalus-client-api\log-config-prod.yaml installers\log-config-prod.yaml
move   node_modules\daedalus-client-api\cardano-node.exe     installers\
move   node_modules\daedalus-client-api\cardano-launcher.exe installers\
move   node_modules\daedalus-client-api\configuration.yaml installers\
move   node_modules\daedalus-client-api\*genesis*.json installers\
del /f node_modules\daedalus-client-api\*.exe
