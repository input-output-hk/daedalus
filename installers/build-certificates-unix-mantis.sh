#!/bin/sh

exec > /tmp/mantis.log 2>&1

pwd
env
mkdir -pv tls/ca
../Resources/app/mantis.app/Contents/MacOS/mantis keytool -genkey -keyalg RSA -alias mantis -keystore conf/mantis.jks -validity 3650 -keysize 2048 -storepass nopassword -keypass nopassword -dname "CN=localhost"
../Resources/app/mantis.app/Contents/MacOS/mantis keytool -keystore conf/mantis.jks -export -alias mantis -file /Applications/DaedalusMantis.app/Contents/MacOS/tls/ca/ca.der -storepass nopassword
openssl x509 -in tls/ca/ca.der -inform der -out tls/ca/ca.crt
ls -ltrh
