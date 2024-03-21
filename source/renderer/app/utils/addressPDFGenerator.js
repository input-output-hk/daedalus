'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.addressPDFGenerator = void 0;
const moment_1 = __importDefault(require('moment'));
const react_intl_1 = require('react-intl');
const generateAddressPDFChannel_1 = require('../ipc/generateAddressPDFChannel');
const global_messages_1 = __importDefault(require('../i18n/global-messages'));
const messages = (0, react_intl_1.defineMessages)({
  creationDate: {
    id: 'wallet.receive.pdf.creationDate',
    defaultMessage: '!!!PDF creation date {date} {time}',
    description: 'PDF title',
  },
  noteLabel: {
    id: 'wallet.receive.pdf.noteLabel',
    defaultMessage: '!!!Note:',
    description: 'PDF title',
  },
  title: {
    id: 'wallet.receive.pdf.title',
    defaultMessage: '!!!Daedalus Cardano ada address',
    description: 'PDF title',
  },
  author: {
    id: 'wallet.receive.pdf.author',
    defaultMessage: '!!!Daedalus wallet',
    description: 'PDF author',
  },
  networkLabel: {
    id: 'wallet.receive.pdf.networkLabel',
    defaultMessage: '!!!Cardano Network:',
    description: 'PDF networkLabel',
  },
});
const addressPDFGenerator = async ({
  address,
  note,
  filePath,
  currentLocale,
  currentDateFormat,
  currentTimeFormat,
  network,
  isMainnet,
  intl,
}) => {
  const date = (0, moment_1.default)().format(currentDateFormat);
  const time = (0, moment_1.default)().format(currentTimeFormat);
  await generateAddressPDFChannel_1.generateAddressPDFChannel.send({
    address,
    filePath,
    note,
    currentLocale,
    isMainnet,
    networkLabel: intl.formatMessage(messages.networkLabel),
    networkName: intl.formatMessage(
      global_messages_1.default[`network_${network}`]
    ),
    creationDate: intl.formatMessage(messages.creationDate, {
      date,
      time,
    }),
    noteLabel: intl.formatMessage(messages.noteLabel),
    title: intl.formatMessage(messages.title),
    author: intl.formatMessage(messages.author),
  });
};
exports.addressPDFGenerator = addressPDFGenerator;
//# sourceMappingURL=addressPDFGenerator.js.map
