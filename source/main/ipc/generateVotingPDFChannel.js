'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.handleVotingPDFRequests = exports.generateVotingPDFChannel = void 0;
const fs_1 = __importDefault(require('fs'));
const path_1 = __importDefault(require('path'));
const pdfkit_1 = __importDefault(require('pdfkit'));
const qr_image_1 = __importDefault(require('qr-image'));
const MainIpcChannel_1 = require('./lib/MainIpcChannel');
const api_1 = require('../../common/ipc/api');
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../common/assets/pdf/NotoSa... Remove this comment to see the full error message
const NotoSans_Regular_ttf_1 = __importDefault(
  require('../../common/assets/pdf/NotoSans-Regular.ttf')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../common/assets/pdf/NotoSa... Remove this comment to see the full error message
const NotoSans_Medium_ttf_1 = __importDefault(
  require('../../common/assets/pdf/NotoSans-Medium.ttf')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../common/assets/pdf/arial-... Remove this comment to see the full error message
const arial_unicode_ttf_1 = __importDefault(
  require('../../common/assets/pdf/arial-unicode.ttf')
);
exports.generateVotingPDFChannel = new MainIpcChannel_1.MainIpcChannel(
  api_1.GENERATE_VOTING_PDF_CHANNEL
);
const handleVotingPDFRequests = () => {
  exports.generateVotingPDFChannel.onReceive(
    (request) =>
      new Promise((resolve, reject) => {
        // Prepare params
        const {
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
        } = request;
        const readAssetSync = (p) =>
          fs_1.default.readFileSync(path_1.default.join(__dirname, p));
        let fontRegular;
        let fontMedium;
        if (currentLocale === 'ja-JP') {
          fontRegular = arial_unicode_ttf_1.default;
          fontMedium = arial_unicode_ttf_1.default;
        } else {
          fontRegular = NotoSans_Regular_ttf_1.default;
          fontMedium = NotoSans_Medium_ttf_1.default;
        }
        // Generate QR image for wallet voting
        const qrCodeImage = qr_image_1.default.imageSync(qrCode, {
          type: 'png',
          size: 10,
          ec_level: 'L',
          margin: 0,
        });
        try {
          const fontBufferMedium = readAssetSync(fontMedium);
          const fontBufferRegular = readAssetSync(fontRegular);
          const textColor = '#5e6066';
          const textColorRed = '#ea4c5b';
          const width = 640;
          const height = 450;
          const doc = new pdfkit_1.default({
            size: [width, height],
            margins: {
              bottom: 20,
              left: 30,
              right: 30,
              top: 20,
            },
            info: {
              Title: title,
              Author: author,
            },
          }).fillColor(textColor);
          // Title
          doc.font(fontBufferMedium).fontSize(18).text(title.toUpperCase(), {
            align: 'center',
            characterSpacing: 2,
          });
          // Creation date
          doc
            .font(fontBufferRegular)
            .fontSize(12)
            .text(creationDate.toUpperCase(), {
              align: 'center',
              characterSpacing: 0.6,
            });
          doc.moveDown();
          // QR Code
          doc.image(qrCodeImage, {
            fit: [width - 60, 192],
            align: 'center',
          });
          doc.moveDown();
          // Wallet name
          doc.font(fontBufferMedium).fontSize(14).text(walletNameLabel, {
            align: 'center',
            characterSpacing: 0.6,
          });
          doc.font(fontBufferRegular).text(walletName, {
            align: 'center',
            characterSpacing: 0.6,
          });
          doc.moveDown();
          // Footer
          if (!isMainnet) {
            doc
              .fontSize(12)
              .font(fontBufferMedium)
              .fillColor(textColorRed)
              .text(`${networkLabel} ${networkName}`, {
                align: 'center',
              });
          }
          // Write file to disk
          const writeStream = fs_1.default.createWriteStream(filePath);
          doc.pipe(writeStream);
          doc.end();
          writeStream.on('close', resolve);
          writeStream.on('error', reject);
        } catch (error) {
          reject(error);
        }
      })
  );
};
exports.handleVotingPDFRequests = handleVotingPDFRequests;
//# sourceMappingURL=generateVotingPDFChannel.js.map
