rem DEPENDENCIES:
rem   1. Node.js ('npm' binary in PATH)
rem   2. 7zip    ('7z'  binary in PATH)
rem
rem   installer dev mode:  set SKIP_TOOLS/SKIP_NODE/SKIP_CARDANO_FETCH/SKIP_STACK/SKIP_LIBS/SKIP_TO_FRONTEND/SKIP_TO_INSTALLER

set /p CLUSTERS=<installer-clusters.cfg

@echo ##############################################################################
@echo ###
@echo ### Will build clusters:  %CLUSTERS%
@echo ###
@echo ##############################################################################

set LIBRESSL_VERSION=2.5.3
set CURL_VERSION=7.54.0

set CURL_URL=https://bintray.com/artifact/download/vszakats/generic/curl-%CURL_VERSION%-win64-mingw.7z
set CURL_BIN=curl-%CURL_VERSION%-win64-mingw\bin
set LIBRESSL_URL=https://ftp.openbsd.org/pub/OpenBSD/LibreSSL/libressl-%LIBRESSL_VERSION%-windows.zip
set DLLS_URL=https://s3.eu-central-1.amazonaws.com/daedalus-ci-binaries/DLLs.zip

@echo Building Daedalus
@echo ..with LibreSSL version:    %LIBRESSL_VERSION%
@echo .

@if not [%SKIP_TO_INSTALLER%]==[] (@echo WARNING: SKIP_TO_INSTALLER active, skipping to frontend packaging
    pushd installers & goto :build_installers)
@if not [%SKIP_TO_FRONTEND%]==[]   (@echo WARNING: SKIP_TO_FRONTEND active, skipping directly to installer rebuild
    pushd installers & goto :build_frontend)

@if not [%SKIP_TOOLS%]==[] (@echo WARNING: SKIP_TOOLS active, skipping installing tools
    goto :after_tools)
@echo ##############################################################################
@echo ###
@echo ### Installing tools:  curl, 7z
@echo ###
@echo ##############################################################################
powershell -Command "try { Import-Module BitsTransfer; Start-BitsTransfer -Source '%CURL_URL%' -Destination 'curl.7z'; } catch { exit 1; }"
@if %errorlevel% neq 0 (@echo FAILED: couldn't obtain curl from %CURL_URL% using BITS
	popd & exit /b 1)
del /f curl.exe curl-ca-bundle.crt libcurl.dll
7z e curl.7z %CURL_BIN%\curl.exe %CURL_BIN%\curl-ca-bundle.crt %CURL_BIN%\libcurl.dll
@if %errorlevel% neq 0 (@echo FAILED: couldn't extract curl from downloaded archive
	popd & exit /b 1)
:after_tools

@if not [%SKIP_NODE%]==[] (@echo WARNING: SKIP_NODE active, skipping Node dependencies
    goto :after_node)
@echo ##############################################################################
@echo ###
@echo ### Installing Node dependencies
@echo ###
@echo ##############################################################################
call npm install
@if %errorlevel% neq 0 (@echo FAILED: npm install
    exit /b 1)
:after_node

@echo ##############################################################################
@echo ###
@echo ### Preparing installer generator
@echo ###
@echo ##############################################################################
pushd installers
    @if not [%SKIP_LIBS%]==[] (@echo WARNING: SKIP_LIBS active, skipping lib installation
        goto :after_libs)
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
:after_libs

    @if not [%SKIP_STACK%]==[] (@echo "WARNING: SKIP_STACK active, skipping stack setup"
        goto :after_stack)
    @echo ##############################################################################
    @echo ###
    @echo ### Installing stack
    @echo ###
    @echo ##############################################################################
    ..\curl --location http://www.stackage.org/stack/windows-x86_64 -o stack.zip
    @if %errorlevel% neq 0 (@echo FAILED: stack couldn't be obtained
        popd & exit /b 1)
    del /f stack.exe 2>nul
    7z x stack.zip stack.exe
    @if %errorlevel% neq 0 (@echo FAILED: couldn't extract stack from the distribution package
        exit /b 1)
    del stack.zip

    stack setup --no-reinstall >/nul
:after_stack

    FOR /F "tokens=* USEBACKQ" %%F IN (`stack path --local-bin`) DO (
        SET PATHEXTN=%%F)
    set PATH=%PATH%;%PATHEXTN%

    @echo ##############################################################################
    @echo ###
    @echo ### Building installer generator
    @echo ###
    @echo ##############################################################################

    call ..\scripts\appveyor-retry stack --no-terminal -j 2 install daedalus-installer
    @if %errorlevel% neq 0 (@echo FATAL: persistent failure while installing daedalus-installer
        popd & exit /b 1)

:build_installers

if NOT DEFINED APPVEYOR_BUILD_NUMBER        ( set APPVEYOR_BUILD_NUMBER=0 )
make-installer --out-dir . appveyor
echo dontfail
@if %errorlevel% neq 0 ( @echo FATAL: failed to build installer
                         popd & exit /b 1)
