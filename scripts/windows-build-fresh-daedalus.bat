rem DEPENDENCIES:
rem   1. Node.js ('npm' binary in PATH)
rem   2. 7zip    ('7z'  binary in PATH)
rem   3. Git     ('git' binary in PATH)

@set DEFAULT_BRANCH=cardano-sl-0.4

set BRANCH=%1
@if [%BRANCH%]==[] (set BRANCH=%DEFAULT_BRANCH%)
set GITHUB_USER=%2
@if [%GITHUB_USER%]==[] (set GITHUB_USER=input-output-hk)

@set URL=https://github.com/%GITHUB_USER%/daedalus.git

move daedalus daedalus.old 2>nul

@echo Building Daedalus branch %BRANCH% from %URL%
git clone %URL%
@if %errorlevel% neq 0 (@echo FAILED: git clone %URL%
	exit /b 1)

@pushd daedalus
    git reset --hard origin/%BRANCH%
    @if %errorlevel% neq 0 (@echo FAILED: git reset --hard origin/%BRANCH%
	exit /b 1)
    @for /f %%a in ('git show-ref --hash HEAD') do set version=%%a
    @call "C:\Program Files (x86)\Microsoft Visual Studio 14.0\Common7\Tools\VsDevCmd.bat"
    call scripts\build-installer-win64 %GITHUB_USER%-%BRANCH%-%version% %BRANCH%
@popd
