rem DEPENDENCIES:
rem   1. Node.js ('npm' binary in PATH)
rem   2. 7zip    ('7z'  binary in PATH)

set DAEDALUS_VERSION=%1
@if [%DAEDALUS_VERSION%]==[] (@echo FATAL: DAEDALUS_VERSION [argument #1] was not provided
    exit /b 1);

@echo Building Daedalus version %DAEDALUS_VERSION%

call npm install
@if %errorlevel% neq 0 (@echo FAILED: npm install
    exit /b 1)

@echo Installing current Cardano
rmdir /s/q node_modules\daedalus-client-api 2>nul
mkdir      node_modules\daedalus-client-api

pushd node_modules\daedalus-client-api
    powershell -Command "try { $wc = New-Object net.webclient; $wc.Downloadfile('https://ci.appveyor.com/api/projects/jagajaga/cardano-sl/artifacts/CardanoSL.zip?branch=cardano-sl-0.4', 'CardanoSL.zip'); } catch { exit 1; }"
    @if %errorlevel% neq 0 (@echo powershell -Command "try { $wc = New-Object net.webclient; $wc.Downloadfile('https://ci.appveyor.com/api/projects/jagajaga/cardano-sl/artifacts/CardanoSL.zip?branch=cardano-sl-0.4', 'CardanoSL.zip'); } catch { exit 1; }"
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
@if %errorlevel% neq 0 (@echo FAILED: npm run package -- --icon installers/icons/64x64
	exit /b 1)

pushd installers
    @echo Installing stack
    powershell -Command "try { $wc = New-Object net.webclient; $wc.Downloadfile('http://www.stackage.org/stack/windows-x86_64', 'stack.zip'); } catch { exit 1; }"
    @if %errorlevel% neq 0 (@echo powershell -Command "try { $wc = New-Object net.webclient; $wc.Downloadfile('http://www.stackage.org/stack/windows-x86_64', 'stack.zip'); } catch { exit 1; }"
	popd & exit /b 1)
    7z x stack.zip stack.exe
    @if %errorlevel% neq 0 (@echo FAILED: 7z x stack.zip stack.exe
	exit /b 1)
    del stack.zip

    @echo Copying DLLs
    @rem TODO: get rocksdb from rocksdb-haskell
    rmdir /s/q DLLs 2>nul
    mkdir      DLLs
    pushd      DLLs
        powershell -Command "try { wget 'https://s3.eu-central-1.amazonaws.com/cardano-sl-testing/DLLs.zip' -outfile DLLs.zip; } catch { exit 1; }"
        @if %errorlevel% neq 0 (@echo FAILED: powershell -Command "try { wget 'https://s3.eu-central-1.amazonaws.com/cardano-sl-testing/DLLs.zip' -outfile DLLs.zip; } catch { exit 1; }"
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
    stack --no-terminal build -j 2 --exec make-installer
    @if %errorlevel% neq 0 (
        @echo
        @echo
        @echo FAILED: stack --no-terminal build -j 2 --exec make-installer
        @echo
        @echo Retrying -- and also waiting for GHC 8.2.1 [see https://github.com/commercialhaskell/stack/issues/2617]
        @echo
        @echo
	goto build
        )
popd

@echo Built Daedalus %DAEDALUS_VERSION%:
@dir /b/s installers\daedalus*
