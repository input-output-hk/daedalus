rem DEPENDENCIES:
rem   1. Node.js ('npm' binary in PATH)
rem   2. 7zip    ('7z'  binary in PATH)

set MIN_CARDANO_BYTES=50000000
set LIBRESSL_VERSION=2.5.3
set CURL_VERSION=7.54.0

set DAEDALUS_VERSION=%1
@if [%DAEDALUS_VERSION%]==[] (@echo FATAL: DAEDALUS_VERSION [argument #1] was not provided
    exit /b 1);
set CARDANO_BRANCH=%2
@if [%CARDANO_BRANCH%]==[]   (@echo NOTE: CARDANO_BRANCH [argument #2] was not provided
    exit /b 1);

set CURL_URL=https://bintray.com/artifact/download/vszakats/generic/curl-%CURL_VERSION%-win64-mingw.7z
set CURL_BIN=curl-%CURL_VERSION%-win64-mingw\bin
set CARDANO_URL=https://ci.appveyor.com/api/projects/jagajaga/cardano-sl/artifacts/CardanoSL.zip?branch=%CARDANO_BRANCH%
set LIBRESSL_URL=https://ftp.openbsd.org/pub/OpenBSD/LibreSSL/libressl-%LIBRESSL_VERSION%-windows.zip
set DLLS_URL=https://s3.eu-central-1.amazonaws.com/cardano-sl-testing/DLLs.zip

@echo Building Daedalus version:  %DAEDALUS_VERSION%
@echo ..with Cardano branch:      %CARDANO_BRANCH%
@echo ..with LibreSSL version:    %LIBRESSL_VERSION%
@echo .

@echo Obtaining curl
powershell -Command "try { Import-Module BitsTransfer; Start-BitsTransfer -Source '%CURL_URL%' -Destination 'curl.7z'; } catch { exit 1; }"
@if %errorlevel% neq 0 (@echo FAILED: couldn't obtain curl from %CURL_URL% using BITS
	popd & exit /b 1)
del /f curl.exe curl-ca-bundle.crt libcurl.dll
7z e curl.7z %CURL_BIN%\curl.exe %CURL_BIN%\curl-ca-bundle.crt %CURL_BIN%\libcurl.dll
@if %errorlevel% neq 0 (@echo FAILED: couldn't extract curl from downloaded archive
	popd & exit /b 1)

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

move   node_modules\daedalus-client-api\log-config-prod.yaml installers\log-config-prod.yaml
move   node_modules\daedalus-client-api\cardano-node.exe     installers\
move   node_modules\daedalus-client-api\cardano-launcher.exe installers\
del /f node_modules\daedalus-client-api\*.exe

@echo Packaging frontend
call npm run package -- --icon installers/icons/64x64
@if %errorlevel% neq 0 (@echo FAILED: Failed to package the frontend
	exit /b 1)
pushd installers
    @echo Installing stack
    ..\curl http://www.stackage.org/stack/windows-x86_64 -o stack.zip
    @if %errorlevel% neq 0 (@echo FAILED: stack couldn't be obtained
	popd & exit /b 1)
    del /f stack.exe
    7z x stack.zip stack.exe
    @if %errorlevel% neq 0 (@echo FAILED: couldn't extract stack from the distribution package
	exit /b 1)
    del stack.zip

    @echo Copying DLLs
    @rem TODO: get rocksdb from rocksdb-haskell
    rmdir /s/q DLLs 2>nul
    mkdir      DLLs
    pushd      DLLs
        ..\..\curl %DLLS_URL% -o DLLs.zip
        @if %errorlevel% neq 0 (@echo FAILED: couldn't obtain CardanoSL DLL package
		exit /b 1)
        7z x DLLs.zip
        @if %errorlevel% neq 0 (@echo FAILED: 7z x DLLs.zip
		popd & popd & exit /b 1)
        del DLLs.zip
    popd

    @echo Building the installer
    stack setup --no-reinstall
    @if %errorlevel% neq 0 (@echo FAILED: stack setup --no-reinstall
	exit /b 1)

:build
    for /l %%x in (1, 1, 5) do (
        stack --no-terminal build -j 2 --exec make-installer
        @if %errorlevel% equ 0 goto :built

        @echo .
        @echo .
        @echo FAILED: stack --no-terminal build -j 2 --exec make-installer
        @echo .
        @timeout 7
        @echo Retrying -- and also waiting for GHC 8.2.1 [see https://github.com/commercialhaskell/stack/issues/2617]
        @echo .
        @echo .
    )
    @echo FATAL: persistent failure while building installer with:  stack --no-terminal build -j 2 --exec make-installer
    exit /b 1
:built
popd

@echo .
@echo Successfully built Daedalus %DAEDALUS_VERSION%
@echo .
@dir /b/s installers\daedalus*
