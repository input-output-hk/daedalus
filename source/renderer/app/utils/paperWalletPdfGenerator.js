'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.paperWalletPdfGenerator = void 0;
const react_intl_1 = require('react-intl');
const generatePaperWalletChannel_1 = require('../ipc/generatePaperWalletChannel');
const messages = (0, react_intl_1.defineMessages)({
  walletAddressLabel: {
    id: 'paper.wallet.pdf.walletAddress.label',
    defaultMessage: '!!!Wallet address',
    description: 'Paper wallet pdf "Wallet address" label.',
  },
  recoveryPhraseLabel: {
    id: 'paper.wallet.pdf.recoveryPhrase.label',
    defaultMessage: '!!!Paper wallet recovery phrase',
    description: 'Paper wallet pdf "Paper wallet recovery phrase" label.',
  },
  passwordLabel: {
    id: 'paper.wallet.pdf.password.label',
    defaultMessage: '!!!Password',
    description: 'Paper wallet pdf "Password" label.',
  },
  infoTitle: {
    id: 'paper.wallet.pdf.info.title',
    defaultMessage: '!!!Daedalus paper wallet certificate',
    description: 'PDF title',
  },
  infoAuthor: {
    id: 'paper.wallet.pdf.info.author',
    defaultMessage: '!!!Daedalus wallet',
    description: 'PDF author',
  },
});
const paperWalletPdfGenerator = async ({
  address,
  filePath,
  mnemonics,
  intl,
  isMainnet,
  buildLabel,
  timestamp,
}) => {
  await generatePaperWalletChannel_1.generatePaperWalletChannel.send({
    address,
    filePath,
    mnemonics,
    isMainnet,
    buildLabel,
    timestamp,
    messages: {
      walletAddressLabel: intl.formatMessage(messages.walletAddressLabel),
      recoveryPhraseLabel: intl.formatMessage(messages.recoveryPhraseLabel),
      infoTitle: intl.formatMessage(messages.infoTitle),
      infoAuthor: intl.formatMessage(messages.infoAuthor),
    },
  });
};
exports.paperWalletPdfGenerator = paperWalletPdfGenerator;
//# sourceMappingURL=paperWalletPdfGenerator.js.map
