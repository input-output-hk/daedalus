const path = require('path');
const { spawn } = require('child_process');

const spawnWatchdog = (configPath = process.env.DAEDALUS_CONFIG_FILE) => {
  if (!configPath || !path.isAbsolute(configPath)) {
    throw new Error('DAEDALUS_CONFIG_FILE must be a non-empty absolute path');
  }

  return spawn('cardano-watchdog', ['--config', configPath], {
    stdio: 'inherit',
  });
};

if (require.main === module) {
  const run = () => {
    let child;
    try {
      child = spawnWatchdog();
    } catch (error) {
      console.error(error instanceof Error ? error.message : error);
      process.exitCode = 1;
      return;
    }

    const forwardSignal = (signal) => child.kill(signal);
    process.once('SIGINT', forwardSignal);
    process.once('SIGTERM', forwardSignal);
    child.once('error', (error) => {
      console.error(`Unable to start cardano-watchdog: ${error.message}`);
      process.exitCode = 1;
    });
    child.once('exit', (code, signal) => {
      process.removeListener('SIGINT', forwardSignal);
      process.removeListener('SIGTERM', forwardSignal);
      if (signal) process.kill(process.pid, signal);
      else process.exitCode = code ?? 1;
    });
  };
  run();
}

module.exports = { spawnWatchdog };
