const { exec } = require('child_process');

if (!process.env.CI) {
  exec('npm run build:electron')
}
