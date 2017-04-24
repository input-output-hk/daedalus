@rem Source: https://pki-tutorial.readthedocs.io/en/latest/simple/index.html
@echo Generating certificates.
@x64\openssl version

@echo .
@echo ============================================================================
@echo [1/6] Creating database
rmdir /s/q tls\ca tls\certs 2>nul

mkdir tls\ca\private tls\ca\db tls\certs
copy nul  tls\ca\db\ca.db
copy nul  tls\ca\db\ca.db.attr
echo 01 > tls\ca\db\ca.crt.srl
@echo ============================================================================

@echo [2/6] Generating password
@echo %RANDOM%  > tls\secret  & echo %RANDOM% >> tls\secret & echo %RANDOM% >> tls\secret & echo %RANDOM% >> tls\secret
@echo %RANDOM% >> tls\secret  & echo %RANDOM% >> tls\secret & echo %RANDOM% >> tls\secret & echo %RANDOM% >> tls\secret
@echo %RANDOM% >> tls\secret  & echo %RANDOM% >> tls\secret & echo %RANDOM% >> tls\secret & echo %RANDOM% >> tls\secret
@echo ============================================================================

@echo [3/6] CA self-sign request
x64\openssl req -new ^
            -config     ca.conf                ^
            -out        tls\ca\ca.csr          ^
            -passout    file:tls\secret        ^
            -keyout     tls\ca\private\ca.key
@if %errorlevel% neq 0 (@echo . & echo "FAILED: CA self-sign request" & exit /b 1)
@echo .
@echo ============================================================================

@echo [4/6] CA certificate
x64\openssl ca  -selfsign -batch ^
            -config     ca.conf                ^
            -in         tls\ca\ca.csr          ^
            -out        tls\ca\ca.crt          ^
            -passin     file:tls\secret        ^
            -extensions ca_ext
@if %errorlevel% neq 0 (@echo . & echo "FAILED: CA self-signed certificate generation" & exit /b 1)
@echo .
@echo ============================================================================

@echo [5/6] Server certificate signing request
x64\openssl req -new ^
            -config     server.conf            ^
            -out        tls\certs\server.csr   ^
            -keyout     tls\certs\server.key
@if %errorlevel% neq 0 (@echo . & echo "FAILED: server certificate signing request" & exit /b 1)
@echo .
@echo ============================================================================

@echo [6/6] Server certificate
x64\openssl ca -batch ^
            -config     ca.conf               ^
            -in         tls\certs\server.csr  ^
            -out        tls\certs\server.crt  ^
            -passin     file:tls\secret       ^
            -extensions server_ext
@if %errorlevel% neq 0 (@echo . & echo "FAILED: server certificate signing" & exit /b 1)
@echo .
@echo ============================================================================

@echo [7/6] Cleanup
del tls\secret ca.conf server.conf
rmdir /s/q tls\ca\private x86 x64

@echo ============================================================================
@echo Oll Korrect
