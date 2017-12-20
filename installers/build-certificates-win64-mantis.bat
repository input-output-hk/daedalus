set OUTDIR=%1
pwd
dir
mkdir tls
mkdir tls\ca
resources\app\mantis\mantis keytool -genkey -keyalg RSA -alias mantis -keystore conf/mantis.jks -validity 3650 -keysize 2048 -storepass nopassword -keypass nopassword -dname "CN=localhost"
resources\app\mantis\mantis keytool -keystore conf/mantis.jks -export -alias mantis -file %OUTDIR%\tls\ca\ca.der -storepass nopassword
x64\openssl x509 -in tls\ca\ca.der -inform der -out tls\ca\ca.crt
