const groupBy = require('lodash/groupBy');
const values = require('lodash/values');
const pick = require('lodash/pick');
// Custom formatter as described here
// https://formatjs.io/docs/tooling/cli#extraction
module.exports = {
  format(messages) {
    const messagesIncludingIds = Object.entries(messages).map(([id, data]) => ({ id, ...data }));
    return values(groupBy(messagesIncludingIds, 'file')).map((fileDescriptor) => ({
      // required format for react-intl-translations-manager
      // https://www.npmjs.com/package/react-intl-translations-manager#provideextractedmessages
      descriptors: fileDescriptor.map(message => {
        return pick(message, ['id', 'defaultMessage', 'description'])
      }),
      path: fileDescriptor[0].file,
    }));
  }
};
