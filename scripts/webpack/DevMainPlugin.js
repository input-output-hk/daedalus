const { exec } = require('child_process');

class DevMainPlugin {

  apply(compiler) {
    let mainCompilation = null;
    compiler.hooks.done.tap(
      'DevMainPlugin',
      () => {
        if (mainCompilation === null) {
          mainCompilation = exec("yarn dev:main", null, (error, stderr, stdout) => {
            console.log(stdout);
            console.error(error, stderr);
          });
          mainCompilation.once('close', () => {
            mainCompilation = null;
          });
        }
      }
    );
  }
}

module.exports = DevMainPlugin;
