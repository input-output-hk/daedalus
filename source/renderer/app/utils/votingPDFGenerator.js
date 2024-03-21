'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.votingPDFGenerator = exports.VotingPDFGeneratorResult = void 0;
const moment_1 = __importDefault(require('moment'));
const path_1 = __importDefault(require('path'));
const react_intl_1 = require('react-intl');
const generateVotingPDFChannel_1 = require('../ipc/generateVotingPDFChannel');
const files_1 = require('../../../common/utils/files');
const show_file_dialog_channels_1 = require('../ipc/show-file-dialog-channels');
const global_messages_1 = __importDefault(require('../i18n/global-messages'));
const messages = (0, react_intl_1.defineMessages)({
  title: {
    id: 'voting.votingRegistration.pdf.title',
    defaultMessage: '!!!Voting registration',
    description: 'PDF title',
  },
  walletNameLabel: {
    id: 'voting.votingRegistration.pdf.walletNameLabel',
    defaultMessage: '!!!Wallet name',
    description: 'PDF wallet name title',
  },
  filename: {
    id: 'voting.votingRegistration.pdf.filename',
    defaultMessage: '!!!voting-registration',
    description: 'PDF filename title',
  },
  networkLabel: {
    id: 'voting.votingRegistration.pdf.networkLabel',
    defaultMessage: '!!!Cardano network:',
    description: 'PDF networkLabel label',
  },
  author: {
    id: 'voting.votingRegistration.pdf.author',
    defaultMessage: '!!!Daedalus wallet',
    description: 'PDF author',
  },
});
var VotingPDFGeneratorResult;
(function (VotingPDFGeneratorResult) {
  VotingPDFGeneratorResult['FileSaved'] = 'FileSaved';
  VotingPDFGeneratorResult['CancelledByUser'] = 'CancelledByUser';
})(
  (VotingPDFGeneratorResult =
    exports.VotingPDFGeneratorResult || (exports.VotingPDFGeneratorResult = {}))
);
const votingPDFGenerator = async ({
  qrCode,
  walletName,
  currentLocale,
  currentDateFormat,
  currentTimeFormat,
  desktopDirectoryPath,
  network,
  isMainnet,
  intl,
}) => {
  // Consolidate data
  const title = intl.formatMessage(messages.title);
  const creationDate = (0, moment_1.default)().format(
    `${currentDateFormat} ${currentTimeFormat}`
  );
  const walletNameLabel = intl.formatMessage(messages.walletNameLabel);
  const networkLabel = intl.formatMessage(messages.networkLabel);
  const networkName = intl.formatMessage(
    global_messages_1.default[`network_${network}`]
  );
  const author = intl.formatMessage(messages.author);
  // Generate the filePath
  const localizedFileName = intl.formatMessage(messages.filename);
  const prefix = `catalyst-${localizedFileName}-${walletName}`;
  const name = (0, files_1.generateFileNameWithTimestamp)({
    prefix,
    extension: '',
    isUTC: false,
  });
  const fileExtension = 'pdf';
  const defaultPath = path_1.default.join(
    desktopDirectoryPath,
    `${name}.${fileExtension}`
  );
  const params = {
    defaultPath,
    filters: [
      {
        name,
        extensions: [fileExtension],
      },
    ],
  };
  const dialogPath = await show_file_dialog_channels_1.showSaveDialogChannel.send(
    params
  );
  const filePath = dialogPath.filePath || '';
  if (dialogPath.canceled || !filePath) {
    return VotingPDFGeneratorResult.CancelledByUser;
  }
  await generateVotingPDFChannel_1.generateVotingPDFChannel.send({
    title,
    currentLocale,
    creationDate,
    qrCode,
    walletNameLabel,
    walletName,
    isMainnet,
    networkLabel,
    networkName,
    filePath,
    author,
  });
  return VotingPDFGeneratorResult.FileSaved;
};
exports.votingPDFGenerator = votingPDFGenerator;
//# sourceMappingURL=votingPDFGenerator.js.map
