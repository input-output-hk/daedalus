#!/bin/sh
#
# This is an install-time script that generates the CA/server key/cert pairs,
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
echo [1/6] Creating database
rm -rf tls/ca tls/certs

mkdir -p  tls/ca/private tls/ca/db tls/certs
touch     tls/ca/db/ca.db
touch     tls/ca/db/ca.db.attr
echo 01 > tls/ca/db/ca.crt.srl
echo ============================================================================

echo [2/6] Generating install-time-only use password for the CA key
echo "$RANDOM$RANDOM$RANDOM$RANDOM$RANDOM$RANDOM$RANDOM$RANDOM$RANDOM$RANDOM$RANDOM$RANDOM$RANDOM$RANDOM$RANDOM$RANDOM" > tls/secret
echo ============================================================================

echo [3/6] CA self-sign request
openssl req -new \
            -config     ca.conf                \
            -out        tls/ca/ca.csr          \
            -passout    file:tls/secret        \
            -keyout     tls/ca/private/ca.key || {
        echo "FAILED: CA self-sign request" >&2; exit 1; }
echo
echo ============================================================================

echo [4/6] CA certificate
openssl ca  -selfsign -batch \
            -config     ca.conf                \
            -in         tls/ca/ca.csr          \
            -out        tls/ca/ca.crt          \
            -passin     file:tls/secret        \
            -extensions ca_ext || {
        echo "FAILED: CA self-signed certificate generation" >&2; exit 1; }
echo
echo ============================================================================

echo [5/6] Server certificate signing request
openssl req -new \
            -config     server.conf            \
            -out        tls/certs/server.csr   \
            -keyout     tls/certs/server.key || {
        echo "FAILED: server certificate signing request" >&2; exit 1; }
echo
echo ============================================================================

echo [6/6] Server certificate
openssl ca -batch \
            -config     ca.conf               \
            -in         tls/certs/server.csr  \
            -out        tls/certs/server.crt  \
            -passin     file:tls/secret       \
            -extensions server_ext || {
        echo "FAILED: server certificate signing" >&2; exit 1; }
echo
echo ============================================================================

echo [7/6] Cleanup
rm tls/secret ca.conf server.conf
rm -rf tls/ca/private

echo ============================================================================
echo Oll Korrect
