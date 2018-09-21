const _process = process;

process.once('loaded', () => {
  global.process = _process;
  global.Buffer = Buffer;
  global.require = require;
});
