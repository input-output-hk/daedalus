const _process = process;

process.once('loaded', () => {
  global.process = {
    env: {
      NODE_ENV: _process.env.NODE_ENV,
      NETWORK: _process.env.NETWORK || 'development'
    },
    umask: _process.umask,
    version: _process.version,
    cwd: _process.cwd,
    platform: _process.platform
  };
  global.Buffer = Buffer;
  global.require = require;
  // ESLint will warn about any use of eval(), even this one
  // eslint-disable-next-line
  global.eval = () => {
    throw new Error('This app does not support window.eval().');
  };
});
