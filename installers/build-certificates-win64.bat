@rem This is an install-time script that generates the CA/server/client key/cert pairs,
@rem for front/backend channel security.
@rem It must be run from the application install directory.
@rem
@rem Source: https://pki-tutorial.readthedocs.io/en/latest/simple/index.html
@rem
@rem WARNING: keep in sync with 'installers/build-certificates-unix.sh'

set OUTDIR=%1

@echo Generating certificates in %OUTDIR%.
@x64\openssl version

@echo .
@echo ============================================================================
@echo [1/10] Creating database in %OUTDIR%
rmdir /s /q          %OUTDIR%\tls\ca %OUTDIR%\tls\server 2>nul
mkdir                %OUTDIR%
copy  /y ca.conf     %OUTDIR%
copy  /y client.conf %OUTDIR%
copy  /y server.conf %OUTDIR%
xcopy /y /s /e  x86  %OUTDIR%\x86
xcopy /y /s /e  x64  %OUTDIR%\x64
cd                   %OUTDIR%

mkdir tls\ca\private tls\ca\db tls\client tls\server
copy nul  tls\ca\db\ca.db
copy nul  tls\ca\db\ca.db.attr
echo 01 > tls\ca\db\ca.crt.srl
@echo ============================================================================

echo [2/10] Choosing OpenSSL message digest
set MD=sha256
echo Elected message digest '%MD%'
echo Updating: ca.conf client.conf server.conf
powershell -Command "(gc ca.conf)     -replace 'OPENSSL_MD', '%MD%' | Out-File -encoding ASCII ca.conf"
powershell -Command "(gc client.conf) -replace 'OPENSSL_MD', '%MD%' | Out-File -encoding ASCII client.conf"
powershell -Command "(gc server.conf) -replace 'OPENSSL_MD', '%MD%' | Out-File -encoding ASCII server.conf"
echo ============================================================================

@echo [3/10] Generating install-time-only use password for the CA key
@echo %RANDOM%%RANDOM%%RANDOM%%RANDOM%%RANDOM%%RANDOM%%RANDOM%%RANDOM%%RANDOM%%RANDOM%%RANDOM%%RANDOM%%RANDOM%%RANDOM%%RANDOM%%RANDOM% > tls\secret
@echo ============================================================================

@echo [4/10] CA self-sign request
x64\openssl req -new ^
            -config     ca.conf                ^
            -out        tls\ca\ca.csr          ^
            -passout    file:tls\secret        ^
            -keyout     tls\ca\private\ca.key
@if %errorlevel% neq 0 (@echo . & echo "FAILED: CA self-sign request" & exit /b 1)
@echo .
@echo ============================================================================

@echo [5/10] CA certificate
x64\openssl ca  -selfsign -batch ^
            -config     ca.conf                ^
            -in         tls\ca\ca.csr          ^
            -out        tls\ca\ca.crt          ^
            -passin     file:tls\secret        ^
            -extensions ca_ext
@if %errorlevel% neq 0 (@echo . & echo "FAILED: CA self-signed certificate generation" & exit /b 1)
@echo .
@echo ============================================================================

@echo [6/10] Server certificate signing request
x64\openssl req -new ^
            -config     server.conf             ^
            -out        tls\server\server.csr   ^
            -keyout     tls\server\server.key
@if %errorlevel% neq 0 (@echo . & echo "FAILED: server certificate signing request" & exit /b 1)
@echo .
@echo ============================================================================

@echo [7/10] Client certificate signing request
x64\openssl req -new ^
            -config     client.conf             ^
            -out        tls\client\client.csr   ^
            -keyout     tls\client\client.key
@if %errorlevel% neq 0 (@echo . & echo "FAILED: client certificate signing request" & exit /b 1)
@echo .
@echo ============================================================================

@echo [8/10] Server certificate
x64\openssl ca -batch ^
            -config     ca.conf                ^
            -in         tls\server\server.csr  ^
            -out        tls\server\server.crt  ^
            -passin     file:tls\secret        ^
            -extensions server_ext
@if %errorlevel% neq 0 (@echo . & echo "FAILED: server certificate signing" & exit /b 1)
@echo .
@echo ============================================================================

@echo [9/10] Client certificate
x64\openssl ca -batch ^
            -config     ca.conf                ^
            -in         tls\client\client.csr  ^
            -out        tls\client\client.crt  ^
            -passin     file:tls\secret        ^
            -extensions client_ext
@if %errorlevel% neq 0 (@echo . & echo "FAILED: client certificate signing" & exit /b 1)
@echo .
@echo ============================================================================

@echo [10/10] Cleanup
del tls\secret ca.conf server.conf client.conf
rmdir /s/q tls\ca\private

@echo ============================================================================
@echo Oll Korrect
