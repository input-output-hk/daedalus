const { exec } = require('child_process');

class ManageElectronProcessPlugin {

  apply(compiler) {
    if (compiler.options.watch) {
      let electron = null;
      compiler.hooks.done.tap(
        'RestartElectronPlugin',
        () => {
          if (electron === null) {
            electron = exec("yarn electron .");
            electron.once('close', () => {
              electron = null;
            });
          } else {
            electron.kill();
            electron = exec("yarn electron .");
          }
        }
      );
    }
  }
}

module.exports = ManageElectronProcessPlugin;
