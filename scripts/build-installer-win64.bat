rem DEPENDENCIES:
rem   1. Node.js ('npm' binary in PATH)
rem   2. 7zip    ('7z'  binary in PATH)
rem
rem   installer dev mode:  set SKIP_TO_FRONTEND/SKIP_TO_INSTALLER

set MIN_CARDANO_BYTES=50000000
set LIBRESSL_VERSION=2.5.3
set CURL_VERSION=7.54.0
set CARDANO_BRANCH_DEFAULT=cardano-sl-1.0
set DAEDALUS_VERSION_DEFAULT=local-dev-build-%CARDANO_BRANCH_DEFAULT%

set DAEDALUS_VERSION=%1
@if [%DAEDALUS_VERSION%]==[] (@echo WARNING: DAEDALUS_VERSION [argument #1] was not provided, defaulting to %DAEDALUS_VERSION_DEFAULT%
    set DAEDALUS_VERSION=%DAEDALUS_VERSION_DEFAULT%);
set CARDANO_BRANCH=%2
@if [%CARDANO_BRANCH%]==[]   (@echo WARNING: CARDANO_BRANCH [argument #2] was not provided, defaulting to %CARDANO_BRANCH_DEFAULT%
    set CARDANO_BRANCH=%CARDANO_BRANCH_DEFAULT%);

set CURL_URL=https://bintray.com/artifact/download/vszakats/generic/curl-%CURL_VERSION%-win64-mingw.7z
set CURL_BIN=curl-%CURL_VERSION%-win64-mingw\bin
set NSISVER=3.02.1
set NSIS_URL=https://downloads.sourceforge.net/project/nsis/NSIS%%203/%NSISVER%/nsis-%NSISVER%-setup.exe
set NSIS_PATCH_URL=https://downloads.sourceforge.net/project/nsis/NSIS%%203/%NSISVER%/nsis-%NSISVER%-strlen_8192.zip
set CARDANO_URL=https://ci.appveyor.com/api/projects/jagajaga/cardano-sl/artifacts/CardanoSL.zip?branch=%CARDANO_BRANCH%
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

@echo Obtaining curl
powershell -Command "try { Import-Module BitsTransfer; Start-BitsTransfer -Source '%CURL_URL%' -Destination 'curl.7z'; } catch { exit 1; }"
@if %errorlevel% neq 0 (@echo FAILED: couldn't obtain curl from %CURL_URL% using BITS
	popd & exit /b 1)
del /f curl.exe curl-ca-bundle.crt libcurl.dll
7z e curl.7z %CURL_BIN%\curl.exe %CURL_BIN%\curl-ca-bundle.crt %CURL_BIN%\libcurl.dll
@if %errorlevel% neq 0 (@echo FAILED: couldn't extract curl from downloaded archive
	popd & exit /b 1)

@echo Obtaining NSIS %NSISVER% with 8k-string patch
del /f nsis-setup.exe nsis-strlen_8192.zip
curl -o nsis-setup.exe       --location %NSIS_URL%
@if %errorlevel% neq 0 (@echo FAILED: curl -o nsis-setup.exe       --location %NSIS_URL%
    exit /b 1)

curl -o nsis-strlen_8192.zip --location %NSIS_PATCH_URL%
@if %errorlevel% neq 0 (@echo FAILED: curl -o nsis-strlen_8192.zip --location %NSIS_PATCH_URL%
    exit /b 1)

nsis-setup.exe /S /SD
@if %errorlevel% neq 0 (@echo FAILED: nsis-setup.exe /S /SD
    exit /b 1)

7z    x nsis-strlen_8192.zip -o"c:\Program Files (x86)\NSIS" -aoa -r
@if %errorlevel% neq 0 (@echo FAILED: 7z    x nsis-strlen_8192.zip -o"c:\Program Files (x86)\NSIS" -aoa -r
    exit /b 1)

@echo Installing NPM
call npm install
@if %errorlevel% neq 0 (@echo FAILED: npm install
    exit /b 1)

@echo Obtaining Cardano from branch %CARDANO_BRANCH%
rmdir /s/q node_modules\daedalus-client-api 2>nul
mkdir      node_modules\daedalus-client-api

pushd node_modules\daedalus-client-api
    del /f CardanoSL.zip 2>nul
    ..\..\curl --location %CARDANO_URL% -o CardanoSL.zip
    @if %errorlevel% neq 0 (@echo FAILED: couldn't obtain the cardano-sl package
	popd & exit /b 1)
    @for /F "usebackq" %%A in ('CardanoSL.zip') do set size=%%~zA
    if %size% lss %MIN_CARDANO_BYTES% (@echo FAILED: CardanoSL.zip is too small: threshold=%MIN_CARDANO_BYTES%, actual=%size% bytes
        popd & exit /b 1)

    7z x CardanoSL.zip -y
    @if %errorlevel% neq 0 (@echo FAILED: 7z x CardanoSL.zip -y
	popd & exit /b 1)
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

:build_frontend
@echo Packaging frontend
call npm run package -- --icon installers/icons/64x64
@if %errorlevel% neq 0 (@echo FAILED: Failed to package the frontend
	exit /b 1)

pushd installers
    del /f LibreSSL.zip 2>nul
    @echo Obtaining LibreSSL %LIBRESSL_VERSION%
    ..\curl %LIBRESSL_URL% -o LibreSSL.zip
    @if %errorlevel% neq 0 (@echo FAILED: LibreSSL couldn't be obtained
	popd & exit /b 1)
    7z x LibreSSL.zip
    @if %errorlevel% neq 0 (@echo FAILED: LibreSSL couldn't be extracted from downloaded archive
	popd & exit /b 1)
    del LibreSSL.zip
    rmdir /s/q libressl
    move libressl-%LIBRESSL_VERSION%-windows libressl

    @echo Installing stack
    ..\curl --location http://www.stackage.org/stack/windows-x86_64 -o stack.zip
    @if %errorlevel% neq 0 (@echo FAILED: stack couldn't be obtained
	popd & exit /b 1)
    del /f stack.exe 2>nul
    7z x stack.zip stack.exe
    @if %errorlevel% neq 0 (@echo FAILED: couldn't extract stack from the distribution package
	exit /b 1)
    del stack.zip

    @echo Copying DLLs
    @rem TODO: get rocksdb from rocksdb-haskell
    rmdir /s/q DLLs 2>nul
    mkdir      DLLs
    pushd      DLLs
        ..\..\curl --location %DLLS_URL% -o DLLs.zip
        @if %errorlevel% neq 0 (@echo FAILED: couldn't obtain CardanoSL DLL package
		exit /b 1)
        7z x DLLs.zip
        @if %errorlevel% neq 0 (@echo FAILED: 7z x DLLs.zip
		popd & popd & exit /b 1)
        del DLLs.zip
    popd

    @echo Building the installer
    stack setup --no-reinstall > nul
    @if %errorlevel% neq 0 (@echo FAILED: stack setup --no-reinstall
	exit /b 1)

:build_installer
    call ..\scripts\appveyor-retry call stack --no-terminal build -j 2 --exec make-installer
    @if %errorlevel% equ 0 goto :built

    @echo FATAL: persistent failure while building installer with:  call stack --no-terminal build -j 2 --exec make-installer
    exit /b 1
:built
@echo SUCCESS: call stack --no-terminal build -j 2 --exec make-installer
popd

@dir /b/s installers\daedalus*
