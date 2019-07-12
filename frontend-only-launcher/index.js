const fs = require('fs')
const rimraf = require('./lib/rimraf')
const createAndWriteX509 = require('./lib/x509')

// Declare our state directory for logging and tls certs
const stateDir = `${process.cwd()}/frontend-only-launcher/state`

// Cleanup previous state directory
rimraf(stateDir)

// Create empty state dir
fs.mkdirSync(stateDir)

// Create TLS certificate location
fs.mkdirSync(`${stateDir}/tls`)
fs.mkdirSync(`${stateDir}/tls/client`)

// Create the logging directory
fs.mkdirSync(`${stateDir}/Logs`)

// Create TLS cert file (see below) in ${stateDir}/tls/client
// ca.crt  client.key  client.pem
createAndWriteX509(`${stateDir}/tls/client`)

// Specific env vars 
const cardanoTlsPath = `${stateDir}/tls`

console.log(`
  Start command:

  CARDANO_TLS_PATH=${cardanoTlsPath} \\
  CARDANO_HOST=localhost \\
  CARDANO_PORT=8088 \\
  LAUNCHER_CONFIG=${process.cwd()}/frontend-only-launcher/launcher-config-base.yaml \\
  STATE_DIR=${stateDir} \\
  yarn dev
`)