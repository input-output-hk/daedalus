const fs = require('fs')
const rimraf = require('./lib/rimraf')
const createAndWriteX509 = require('./lib/x509')
const { exec, spawn } = require('child_process')

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

// Determine ngrok proxy
exec('docker exec cardano-byron-docker_proxy_1 curl -s localhost:4040/api/tunnels', (err, stdout) => {
  if (err) {
    throw err
  }

  const data = JSON.parse(stdout)
  const url = data.tunnels.find(el => el.proto === 'http').public_url
    .split('http://')[1]

  if (process.platform === 'darwin') {
    console.log('On Darwin, running `yarn dev`')
    spawn(`yarn`, ['dev'], {
      stdio: ['inherit', 'inherit', 'inherit'],
      env: {
        PATH: process.env.PATH,
        CARDANO_TLS_PATH: cardanoTlsPath,
        CARDANO_HOST: url,
        CARDANO_PORT: 80,
        LAUNCHER_CONFIG: `${process.cwd()}/frontend-only-launcher/launcher-config-base.yaml`,
        STATE_DIR: stateDir
      }
    })
  } else {
    console.log(`
      On ${process.platform}, run the command below:

      CARDANO_TLS_PATH=${cardanoTlsPath} \\
      CARDANO_HOST=${url} \\
      CARDANO_PORT=80 \\
      LAUNCHER_CONFIG=${process.cwd()}/frontend-only-launcher/launcher-config-base.yaml \\
      STATE_DIR=${stateDir} \\
      yarn dev
    `)

    process.exit()
  }
})
