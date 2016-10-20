// This is essentially bulk require
const req = require.context('./locales', true, /\.json.*$/);
const exports = {};

req.keys().forEach((file) => {
  const locale = file.replace('./', '').replace('.json', '');
  exports[locale] = req(file);
});

module.exports = exports;
