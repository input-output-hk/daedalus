rem DEPENDENCIES:
rem   1. Node.js ('npm' binary in PATH)
rem   2. 7zip    ('7z'  binary in PATH)
rem
rem   installer dev mode:  set SKIP_TO_FRONTEND/SKIP_TO_INSTALLER

set MIN_CARDANO_BYTES=20000000
set LIBRESSL_VERSION=2.5.3
set CURL_VERSION=7.54.0
set CARDANO_BRANCH_DEFAULT=release/1.1.0
set DAEDALUS_VERSION_DEFAULT=local-dev-build-%CARDANO_BRANCH_DEFAULT%

set DAEDALUS_VERSION=%1
@if [%DAEDALUS_VERSION%]==[] (@echo WARNING: DAEDALUS_VERSION [argument #1] was not provided, defaulting to %DAEDALUS_VERSION_DEFAULT%
    set DAEDALUS_VERSION=%DAEDALUS_VERSION_DEFAULT%);
set CARDANO_BRANCH=%2
@if [%CARDANO_BRANCH%]==[]   (@echo WARNING: CARDANO_BRANCH [argument #2] was not provided, defaulting to %CARDANO_BRANCH_DEFAULT%
    set CARDANO_BRANCH=%CARDANO_BRANCH_DEFAULT%);

set CURL_URL=https://bintray.com/artifact/download/vszakats/generic/curl-%CURL_VERSION%-win64-mingw.7z
set CURL_BIN=curl-%CURL_VERSION%-win64-mingw\bin
set CARDANO_URL=https://ci.appveyor.com/api/projects/input-output/cardano-sl/artifacts/CardanoSL.zip?branch=%CARDANO_BRANCH%
set LIBRESSL_URL=https://ftp.openbsd.org/pub/OpenBSD/LibreSSL/libressl-%LIBRESSL_VERSION%-windows.zip
set DLLS_URL=https://s3.eu-central-1.amazonaws.com/daedalus-ci-binaries/DLLs.zip

@echo Building Daedalus version:  %DAEDALUS_VERSION%
@echo ..with Cardano branch:      %CARDANO_BRANCH%
@echo ..with LibreSSL version:    %LIBRESSL_VERSION%
@echo .

@if not [%SKIP_TO_INSTALLER%]==[] (@echo WARNING: SKIP_TO_INSTALLER set, skipping to frontend packaging       
    pushd installers & goto :build_installer)
@if not [%SKIP_TO_FRONTEND%]==[]   (@echo WARNING: SKIP_TO_FRONTEND set, skipping directly to installer rebuild
    pushd installers & goto :build_frontend)

rem @echo Obtaining curl
rem powershell -Command "try { Import-Module BitsTransfer; Start-BitsTransfer -Source '%CURL_URL%' -Destination 'curl.7z'; } catch { exit 1; }"
rem @if %errorlevel% neq 0 (@echo FAILED: couldn't obtain curl from %CURL_URL% using BITS
rem 	popd & exit /b 1)
rem del /f curl.exe curl-ca-bundle.crt libcurl.dll
rem 7z e curl.7z %CURL_BIN%\curl.exe %CURL_BIN%\curl-ca-bundle.crt %CURL_BIN%\libcurl.dll
rem @if %errorlevel% neq 0 (@echo FAILED: couldn't extract curl from downloaded archive
rem 	popd & exit /b 1)

rem @echo Obtaining NSIS %NSISVER% with 8k-string patch
rem del /f nsis-setup.exe nsis-strlen_8192.zip
rem curl -o nsis-setup.exe       --location %NSIS_URL%
rem @if %errorlevel% neq 0 (@echo FAILED: curl -o nsis-setup.exe       --location %NSIS_URL%
rem     exit /b 1)

rem curl -o nsis-strlen_8192.zip --location %NSIS_PATCH_URL%
rem @if %errorlevel% neq 0 (@echo FAILED: curl -o nsis-strlen_8192.zip --location %NSIS_PATCH_URL%
rem     exit /b 1)

rem nsis-setup.exe /S /SD
rem @if %errorlevel% neq 0 (@echo FAILED: nsis-setup.exe /S /SD
rem     exit /b 1)

rem 7z    x nsis-strlen_8192.zip -o"c:\Program Files (x86)\NSIS" -aoa -r
rem @if %errorlevel% neq 0 (@echo FAILED: 7z    x nsis-strlen_8192.zip -o"c:\Program Files (x86)\NSIS" -aoa -r
rem     exit /b 1)

rem @echo Installing NPM
rem call npm install
rem @if %errorlevel% neq 0 (@echo FAILED: npm install
rem     exit /b 1)

rem @echo Obtaining Cardano from branch %CARDANO_BRANCH%
rem rmdir /s/q node_modules\daedalus-client-api 2>nul
rem mkdir      node_modules\daedalus-client-api

rem pushd node_modules\daedalus-client-api
rem     del /f CardanoSL.zip 2>nul
rem     ..\..\curl --location %CARDANO_URL% -o CardanoSL.zip
rem     @if %errorlevel% neq 0 (@echo FAILED: couldn't obtain the cardano-sl package
rem 	popd & exit /b 1)
rem     @for /F "usebackq" %%A in ('CardanoSL.zip') do set size=%%~zA
rem     if %size% lss %MIN_CARDANO_BYTES% (@echo FAILED: CardanoSL.zip is too small: threshold=%MIN_CARDANO_BYTES%, actual=%size% bytes
rem         popd & exit /b 1)

rem     7z x CardanoSL.zip -y
rem     @if %errorlevel% neq 0 (@echo FAILED: 7z x CardanoSL.zip -y
rem 	popd & exit /b 1)
rem     del CardanoSL.zip
rem popd

rem @echo cardano-sl build-id:
rem type node_modules\daedalus-client-api\build-id
rem @echo cardano-sl commit-id:
rem type node_modules\daedalus-client-api\commit-id
rem @echo cardano-sl ci-url:
rem type node_modules\daedalus-client-api\ci-url

rem move   node_modules\daedalus-client-api\log-config-prod.yaml installers\log-config-prod.yaml
rem move   node_modules\daedalus-client-api\cardano-node.exe     installers\
rem move   node_modules\daedalus-client-api\cardano-launcher.exe installers\
rem move   node_modules\daedalus-client-api\configuration.yaml installers\
rem move   node_modules\daedalus-client-api\*genesis*.json installers\
rem del /f node_modules\daedalus-client-api\*.exe

rem :build_frontend
rem @echo Packaging frontend
rem call npm run package -- --icon installers/icons/64x64
rem @if %errorlevel% neq 0 (@echo FAILED: Failed to package the frontend
rem 	exit /b 1)

pushd installers
    rem del /f LibreSSL.zip 2>nul
    rem @echo Obtaining LibreSSL %LIBRESSL_VERSION%
    rem ..\curl %LIBRESSL_URL% -o LibreSSL.zip
    rem @if %errorlevel% neq 0 (@echo FAILED: LibreSSL couldn't be obtained
    rem     popd & exit /b 1)
    rem 7z x LibreSSL.zip
    rem @if %errorlevel% neq 0 (@echo FAILED: LibreSSL couldn't be extracted from downloaded archive
    rem     popd & exit /b 1)
    rem del LibreSSL.zip
    rem rmdir /s/q libressl
    rem move libressl-%LIBRESSL_VERSION%-windows libressl

    rem @echo Installing stack
    rem ..\curl --location http://www.stackage.org/stack/windows-x86_64 -o stack.zip
    rem @if %errorlevel% neq 0 (@echo FAILED: stack couldn't be obtained
    rem     popd & exit /b 1)
    rem del /f stack.exe 2>nul
    rem 7z x stack.zip stack.exe
    rem @if %errorlevel% neq 0 (@echo FAILED: couldn't extract stack from the distribution package
    rem     exit /b 1)
    rem del stack.zip

    rem @echo Copying DLLs
    rem @rem TODO: get rocksdb from rocksdb-haskell
    rem rmdir /s/q DLLs 2>nul
    rem mkdir      DLLs
    rem pushd      DLLs
    rem     ..\..\curl --location %DLLS_URL% -o DLLs.zip
    rem     @if %errorlevel% neq 0 (@echo FAILED: couldn't obtain CardanoSL DLL package
    rem     	exit /b 1)
    rem     7z x DLLs.zip
    rem     @if %errorlevel% neq 0 (@echo FAILED: 7z x DLLs.zip
    rem     	popd & popd & exit /b 1)
    rem     del DLLs.zip
    rem popd

    @echo Building the installer
    stack setup --no-reinstall
    @if %errorlevel% neq 0 (@echo FAILED: stack setup --no-reinstall
	exit /b 1)

:install_dhall
    call ..\scripts\appveyor-retry call stack install dhall dhall-json
    @if %errorlevel% equ 0 goto :generate_config
    @echo FATAL: persistent failure while installing dhall
    exit /b 1
:generate_config
    set OS=win64
    set CLUSTER=staging
    pushd ..\config
    call emit-config-windows.bat %OS% %CLUSTER% launcher
    call emit-config-windows.bat %OS% %CLUSTER% wallet-topology
    @if %errorlevel% equ 0 (popd & goto :build_installer)
    @echo FATAL: persistent failure while installing dhall-json
    

:build_installer
rem     call ..\scripts\appveyor-retry call stack --no-terminal build -j 2 --exec make-installer
rem     @if %errorlevel% equ 0 goto :built

rem     @echo FATAL: persistent failure while building installer with:  call stack --no-terminal build -j 2 --exec make-installer
rem     exit /b 1
:built
@echo SUCCESS: call stack --no-terminal build -j 2 --exec make-installer
popd

@dir /b/s installers\daedalus*
