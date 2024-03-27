'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.handleAddressPDFRequests = exports.generateAddressPDFChannel = void 0;
const fs_1 = __importDefault(require('fs'));
const path_1 = __importDefault(require('path'));
const pdfkit_1 = __importDefault(require('pdfkit'));
const qr_image_1 = __importDefault(require('qr-image'));
const MainIpcChannel_1 = require('./lib/MainIpcChannel');
const api_1 = require('../../common/ipc/api');
const pdf_1 = require('../utils/pdf');
const NotoSans_Regular_ttf_1 = __importDefault(
  require('../../common/assets/pdf/NotoSans-Regular.ttf')
);
const NotoSans_Medium_ttf_1 = __importDefault(
  require('../../common/assets/pdf/NotoSans-Medium.ttf')
);
const arial_unicode_ttf_1 = __importDefault(
  require('../../common/assets/pdf/arial-unicode.ttf')
);
const RobotoMono_Regular_ttf_1 = __importDefault(
  require('../../common/assets/pdf/RobotoMono-Regular.ttf')
);
exports.generateAddressPDFChannel = new MainIpcChannel_1.MainIpcChannel(
  api_1.GENERATE_ADDRESS_PDF_CHANNEL
);
const handleAddressPDFRequests = () => {
  exports.generateAddressPDFChannel.onReceive(
    (request) =>
      new Promise((resolve, reject) => {
        // Prepare params
        const {
          address,
          filePath,
          note,
          currentLocale,
          isMainnet,
          networkLabel,
          networkName,
          creationDate,
          noteLabel,
          title,
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
        // Generate QR image for wallet address
        const qrCodeImage = qr_image_1.default.imageSync(address, {
          type: 'png',
          size: 10,
          ec_level: 'L',
          margin: 0,
        });
        try {
          const fontBufferMedium = readAssetSync(fontMedium);
          const fontBufferRegular = readAssetSync(fontRegular);
          const fontBufferMono = readAssetSync(
            RobotoMono_Regular_ttf_1.default
          );
          const fontBufferUnicode = readAssetSync(arial_unicode_ttf_1.default);
          let noteHeight = 0;
          if (note) {
            noteHeight =
              (0, pdf_1.getHeightOfString)(note, fontBufferRegular, 14) + 30;
          }
          const textColor = '#5e6066';
          const textColorRed = '#ea4c5b';
          const width = 640;
          const height = 450 + noteHeight;
          const doc = new pdfkit_1.default({
            size: [width, height],
            margins: {
              bottom: 20,
              left: 20,
              right: 20,
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
          // Address
          doc.font(fontBufferMono).fontSize(9).text(address, {
            align: 'center',
          });
          if (note) {
            doc.moveDown();
            // Note title
            doc.font(fontBufferRegular).fontSize(12).text(noteLabel);
            // Note
            doc.font(fontBufferUnicode).fontSize(12).text(note);
          }
          doc.moveDown();
          // Footer
          if (!isMainnet) {
            doc
              .fontSize(12)
              .font(fontBufferMedium)
              .fillColor(textColorRed)
              .text(`${networkLabel} ${networkName}`, {
                align: 'right',
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
exports.handleAddressPDFRequests = handleAddressPDFRequests;
//# sourceMappingURL=generateAddressPDFChannel.js.map
