@echo downloading mantis
curl --location https://s3-eu-west-1.amazonaws.com/iohk.mantis.installer/daedalus-rc1/windows/mantis-daedalus-1.0-win.zip -o mantis.zip
@if %errorlevel% neq 0 (@echo FAILED: couldn't obtain the mantis package
    exit /b 1)
7z x mantis.zip -y
move mantis\ installers\
del mantis.zip
