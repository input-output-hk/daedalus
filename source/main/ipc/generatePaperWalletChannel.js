'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.handlePaperWalletRequests = exports.generatePaperWalletChannel = void 0;
const fs_1 = __importDefault(require('fs'));
const path_1 = __importDefault(require('path'));
const pdfkit_1 = __importDefault(require('pdfkit'));
const qr_image_1 = __importDefault(require('qr-image'));
const MainIpcChannel_1 = require('./lib/MainIpcChannel');
const api_1 = require('../../common/ipc/api');
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../common/assets/pdf/paper-... Remove this comment to see the full error message
const paper_wallet_certificate_font_ttf_1 = __importDefault(
  require('../../common/assets/pdf/paper-wallet-certificate-font.ttf')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../common/assets/pdf/paper-... Remove this comment to see the full error message
const paper_wallet_certificate_page_1_png_1 = __importDefault(
  require('../../common/assets/pdf/paper-wallet-certificate-page-1.png')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../common/assets/pdf/paper-... Remove this comment to see the full error message
const paper_wallet_certificate_page_1_testnet_png_1 = __importDefault(
  require('../../common/assets/pdf/paper-wallet-certificate-page-1-testnet.png')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../common/assets/pdf/paper-... Remove this comment to see the full error message
const paper_wallet_certificate_page_2_png_1 = __importDefault(
  require('../../common/assets/pdf/paper-wallet-certificate-page-2.png')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../common/assets/pdf/paper-... Remove this comment to see the full error message
const paper_wallet_certificate_page_2_testnet_png_1 = __importDefault(
  require('../../common/assets/pdf/paper-wallet-certificate-page-2-testnet.png')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../common/assets/pdf/paper-... Remove this comment to see the full error message
const paper_wallet_certificate_background_png_1 = __importDefault(
  require('../../common/assets/pdf/paper-wallet-certificate-background.png')
);
exports.generatePaperWalletChannel = new MainIpcChannel_1.MainIpcChannel(
  api_1.GENERATE_PAPER_WALLET_CHANNEL
);
const handlePaperWalletRequests = () => {
  exports.generatePaperWalletChannel.onReceive(
    (request) =>
      new Promise((resolve, reject) => {
        // Prepare params
        const {
          address,
          mnemonics,
          buildLabel,
          filePath,
          isMainnet,
          messages,
          timestamp,
        } = request;
        // Helpers
        const printMnemonic = (index) => `${index + 1}. ${mnemonics[index]}`;
        const readAssetSync = (p) =>
          fs_1.default.readFileSync(path_1.default.join(__dirname, p));
        // Generate QR image for wallet address
        const qrCodeImage = qr_image_1.default.imageSync(address, {
          type: 'png',
          size: 10,
          ec_level: 'L',
          margin: 0,
        });
        const textColor = '#3b5c9b';
        const width = 595.28;
        const height = 841.98;
        const doc = new pdfkit_1.default({
          size: [width, height],
          margins: {
            bottom: 0,
            left: 0,
            right: 0,
            top: 0,
          },
          info: {
            Title: messages.infoTitle,
            Author: messages.infoAuthor,
          },
        });
        try {
          // font family
          const fontBuffer = readAssetSync(
            paper_wallet_certificate_font_ttf_1.default
          );
          doc.font(fontBuffer);
          // background images
          const backgroundUri = readAssetSync(
            paper_wallet_certificate_background_png_1.default
          );
          doc.image(backgroundUri, 0, 0, {
            fit: [width, height],
          });
          // first page
          const page1Uri = readAssetSync(
            isMainnet
              ? paper_wallet_certificate_page_1_png_1.default
              : paper_wallet_certificate_page_1_testnet_png_1.default
          );
          doc.fillColor(textColor);
          // Timestamp
          doc.fontSize(8).text(timestamp, 119, 484);
          doc.image(page1Uri, 0, 0, {
            fit: [width, height],
          });
          doc.rotate(180, {
            origin: [width / 2, height / 2],
          });
          doc.fontSize(10).text(messages.walletAddressLabel, 0, 160, {
            width: 595,
            align: 'center',
          });
          doc.image(qrCodeImage, width / 2 - 80 / 2, 180, {
            fit: [80, 80],
          });
          doc.fontSize(8).text(address, (width - 250) / 2, 274, {
            width: 250,
            align: 'center',
            lineGap: 2,
          });
          // revert document rotation
          doc.rotate(-180, {
            origin: [width / 2, height / 2],
          });
          // second page
          doc.addPage();
          const page2Uri = readAssetSync(
            isMainnet
              ? paper_wallet_certificate_page_2_png_1.default
              : paper_wallet_certificate_page_2_testnet_png_1.default
          );
          doc.image(page2Uri, 0, 0, {
            fit: [width, height],
          });
          doc.rotate(180, {
            origin: [width / 2, height / 2],
          });
          doc.fillColor(textColor);
          doc.fontSize(10).text(messages.recoveryPhraseLabel, 0, 535, {
            width: 595,
            align: 'center',
          });
          // mnemonics
          doc.fontSize(7);
          doc.text(printMnemonic(0), 168, 560);
          doc.text(printMnemonic(1), 212, 560);
          doc.text(printMnemonic(2), 256, 560);
          doc.text(printMnemonic(3), 300, 560);
          doc.text(printMnemonic(4), 344, 560);
          doc.text(printMnemonic(5), 388, 560);
          doc.text(printMnemonic(6), 168, 581);
          doc.text(printMnemonic(7), 212, 581);
          doc.text(printMnemonic(8), 256, 581);
          doc.text(printMnemonic(9), 300, 581);
          doc.text(printMnemonic(10), 344, 581);
          doc.text(printMnemonic(11), 388, 581);
          doc.text(printMnemonic(12), 168, 602);
          doc.text(printMnemonic(13), 212, 602);
          doc.text(printMnemonic(14), 256, 602);
          doc.text(printMnemonic(15), 300, 602);
          doc.text(printMnemonic(16), 344, 602);
          doc.text(printMnemonic(17), 388, 602);
          doc.fontSize(7).text(buildLabel, (width - 270) / 2, 705, {
            width: 270,
            align: 'left',
          });
          doc.rotate(-180, {
            origin: [width / 2, height / 2],
          });
        } catch (error) {
          reject(error);
        }
        // Write file to disk
        const writeStream = fs_1.default.createWriteStream(filePath);
        doc.pipe(writeStream);
        doc.end();
        writeStream.on('close', resolve);
        writeStream.on('error', reject);
      })
  );
};
exports.handlePaperWalletRequests = handlePaperWalletRequests;
//# sourceMappingURL=generatePaperWalletChannel.js.map
