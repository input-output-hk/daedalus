set OS=win64
set CLUSTER=%1

@echo ##############################################################################
@echo ###
@echo ### Building installers:  %CLUSTER%
@echo ###
@echo ##############################################################################

pushd config
    set LAUNCHER="./launcher.dhall ( ./launcher-%OS%.dhall ./%CLUSTER%.dhall ) "
    set TOPOLOGY="./topology.dhall                         ./%CLUSTER%.dhall"
    @echo ##############################################################################
    @echo ###
    @echo ### Generating configs:  %CLUSTER%
    @echo ###
    @echo ###  LAUNCHER=%LAUNCHER:"=%
    @echo ###  TOPOLOGY=%TOPOLOGY:"=%
    @echo ###
    @echo ##############################################################################
    del /f ..\installers\launcher-config.yaml 2>nul
    del /f ..\installers\wallet-topology.yaml 2>nul

    echo %LAUNCHER:"=% | call dhall-to-yaml > ..\installers\launcher-config.yaml
        @if %errorlevel% equ 0 goto after_dty0
        echo FAILED to generate installers\launcher-config.yaml
        popd & exit /b 1
:after_dty0

    echo %TOPOLOGY:"=% | call dhall-to-yaml > ..\installers\wallet-topology.yaml
        @if %errorlevel% equ 0 goto after_dty1
        echo FAILED to generate installers\wallet-topology.yaml
        popd & exit /b 1
:after_dty1

    @echo ##############################################################################
    @echo ###
    @echo ### Generating frontend:  %CLUSTER%
    @echo ###
    @echo ##############################################################################
    call npm run package -- --icon installers/icons/64x64
        @if %errorlevel% equ 0 goto after_npmrun
        @echo FAILED: Failed to package the frontend
        popd & exit /b 1
:after_npmrun
popd

pushd installers
@echo ##############################################################################
@echo ###
@echo ### Building installer:  %CLUSTER%
@echo ###
@echo ##############################################################################
set DAEDALUS_CLUSTER=%CLUSTER%
call ..\scripts\appveyor-retry call stack --no-terminal build -j 2 --exec make-installer
    @if %errorlevel% equ 0 goto after_makeinst
    @echo FATAL: persistent failure while building installer with:  call stack --no-terminal build -j 2 --exec make-installer
    popd & exit /b 1
:after_makeinst

popd
@dir /b/s installers\daedalus*
