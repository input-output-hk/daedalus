const { exec } = require('child_process');

class ManageElectronProcessPlugin {

  apply(compiler) {
    if (compiler.options.watch) {
      let electronMainProcess = null;
      let isMainProcessBeingRestarted = false;
      compiler.hooks.done.tap(
        'RestartElectronPlugin',
        () => {
          if (electronMainProcess === null) {
            electronMainProcess = exec("yarn electron .");
            electronMainProcess.once('close', () => {
              electronMainProcess = null;
              if (isMainProcessBeingRestarted) {
                electronMainProcess = exec("yarn electron .");
                isMainProcessBeingRestarted = false;
              }
            });
          } else if (!isMainProcessBeingRestarted) {
            isMainProcessBeingRestarted = true;
            electronMainProcess.kill();
          }
        }
      );
    }
  }
}

module.exports = ManageElectronProcessPlugin;
