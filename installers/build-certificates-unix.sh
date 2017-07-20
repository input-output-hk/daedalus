#!/bin/sh
#
# This is an install-time script that generates the CA/server/client key/cert pairs,
# for front/backend channel security.
# It must be run from the application install directory.
#
# Source: https://pki-tutorial.readthedocs.io/en/latest/simple/index.html
#
# WARNING: keep in sync with 'installers/build-certificates-win64.bat'

echo Generating certificates.
openssl version

echo
echo ============================================================================
echo [1/9] Creating database
rm -rf tls/ca tls/client tls/server

mkdir -p  tls/ca/private tls/ca/db tls/client tls/server
touch     tls/ca/db/ca.db
touch     tls/ca/db/ca.db.attr
echo 01 > tls/ca/db/ca.crt.srl
echo ============================================================================

echo [2/9] Generating install-time-only use password for the CA key
echo "$RANDOM$RANDOM$RANDOM$RANDOM$RANDOM$RANDOM$RANDOM$RANDOM$RANDOM$RANDOM$RANDOM$RANDOM$RANDOM$RANDOM$RANDOM$RANDOM" > tls/secret
echo ============================================================================

echo [3/9] CA self-sign request
openssl req -new \
            -config     ca.conf                \
            -out        tls/ca/ca.csr          \
            -passout    file:tls/secret        \
            -keyout     tls/ca/private/ca.key || {
        echo "FAILED: CA self-sign request" >&2; exit 1; }
echo
echo ============================================================================

echo [4/9] CA certificate
openssl ca  -selfsign -batch \
            -config     ca.conf                \
            -in         tls/ca/ca.csr          \
            -out        tls/ca/ca.crt          \
            -passin     file:tls/secret        \
            -extensions ca_ext || {
        echo "FAILED: CA self-signed certificate generation" >&2; exit 1; }
echo
echo ============================================================================

echo [5/9] Server certificate signing request
openssl req -new \
            -config     server.conf             \
            -out        tls/server/server.csr   \
            -keyout     tls/server/server.key || {
        echo "FAILED: server certificate signing request" >&2; exit 1; }
echo
echo ============================================================================

echo [6/9] Client certificate signing request
openssl req -new \
            -config     client.conf             \
            -out        tls/client/client.csr   \
            -keyout     tls/client/client.key || {
        echo "FAILED: client certificate signing request" >&2; exit 1; }
echo
echo ============================================================================

echo [7/9] Server certificate
openssl ca -batch \
            -config     ca.conf                \
            -in         tls/server/server.csr  \
            -out        tls/server/server.crt  \
            -passin     file:tls/secret        \
            -extensions server_ext || {
        echo "FAILED: server certificate signing" >&2; exit 1; }
echo
echo ============================================================================

echo [8/9] client certificate
openssl ca -batch \
            -config     ca.conf                \
            -in         tls/client/client.csr  \
            -out        tls/client/client.crt  \
            -passin     file:tls/secret        \
            -extensions client_ext || {
        echo "FAILED: client certificate signing" >&2; exit 1; }
echo
echo ============================================================================

echo [9/9] Cleanup
rm tls/secret
rm -rf tls/ca/private

# ..if launched by OS X installer:
if test ! -z "${INSTALL_PKG_SESSION_ID}"
then rm -f ca.conf server.conf client.conf
fi

echo ============================================================================
echo Oll Korrect
