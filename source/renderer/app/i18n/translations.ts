// This is essentially bulk require
const req = require.context('./locales', true, /\.json.*$/);

const translations = {};
req.keys().forEach((file) => {
  const locale = file.replace('./', '').replace('.json', '');
  translations[locale] = req(file);
});
export default translations;
