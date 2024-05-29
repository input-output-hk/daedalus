@echo off
SETLOCAL EnableExtensions
set "system=x86_64-windows"

echo Deleting all current '*.node' files before rebuilding for Electron's ABI:
for /R %%i in (*.node) do if not "%%~pi"=="*\\@swc*\\*" del /Q /F "%%i"

call npx electron-rebuild --force
call npx electron-rebuild -w usb-detection --force -s
echo ===== all *.node files after 'electron-rebuild': =====
powershell -Command "Get-ChildItem -Recurse -Filter *.node | Sort-Object FullName | ForEach-Object {Get-FileHash $_.FullName -Algorithm SHA1}"
echo ========================================================
ENDLOCAL
