/* eslint-disable no-console */
const fs = require('fs')
const { spawn } = require('child_process')
const rimraf = require('./lib/rimraf')
const createAndWriteX509 = require('./lib/x509')

const { WALLET_HOST, WALLET_PORT } = process.env
if (!WALLET_HOST || !WALLET_PORT) {
  throw new Error('You must set WALLET_HOST & WALLET_PORT on the env to use this script')
}

// Declare our state directory for logging and tls certs
const stateDir = `${process.cwd()}/utils/js-launcher/state`

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

if (process.platform === 'darwin') {
  console.log('On Darwin, running `yarn dev`')
  spawn(`yarn`, ['dev'], {
    stdio: ['inherit', 'inherit', 'inherit'],
    env: {
      PATH: process.env.PATH,
      CARDANO_TLS_PATH: cardanoTlsPath,
      CARDANO_HOST: WALLET_HOST,
      CARDANO_PORT: WALLET_PORT,
      LAUNCHER_CONFIG: `${process.cwd()}/utils/js-launcher/launcher-config-base.yaml`,
      STATE_DIR: stateDir
    }
  })
} else {
  console.log(`
    On ${process.platform}, run the command below:

    CARDANO_TLS_PATH=${cardanoTlsPath} \\
    CARDANO_HOST=${WALLET_HOST} \\
    CARDANO_PORT=${WALLET_PORT} \\
    LAUNCHER_CONFIG=${process.cwd()}/utils/js-launcher/launcher-config-base.yaml \\
    STATE_DIR=${stateDir} \\
    yarn dev
  `)

  process.exit()
}
