import fs from 'fs';
import path from 'path';
import PDFDocument from 'pdfkit';
import qr from 'qr-image';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { GENERATE_ADDRESS_PDF_CHANNEL } from '../../common/ipc/api';
import { getHeightOfString } from '../utils/pdf';
import type {
  GenerateAddressPDFRendererRequest,
  GenerateAddressPDFMainResponse,
} from '../../common/ipc/api';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../common/assets/pdf/NotoSa... Remove this comment to see the full error message
import fontRegularEn from '../../common/assets/pdf/NotoSans-Regular.ttf';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../common/assets/pdf/NotoSa... Remove this comment to see the full error message
import fontMediumEn from '../../common/assets/pdf/NotoSans-Medium.ttf';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../common/assets/pdf/arial-... Remove this comment to see the full error message
import fontUnicode from '../../common/assets/pdf/arial-unicode.ttf';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../common/assets/pdf/NotoSa... Remove this comment to see the full error message
import fontMono from '../../common/assets/pdf/NotoSansMono-Regular.ttf';

export const generateAddressPDFChannel: // IpcChannel<Incoming, Outgoing>
MainIpcChannel<
  GenerateAddressPDFRendererRequest,
  GenerateAddressPDFMainResponse
> = new MainIpcChannel(GENERATE_ADDRESS_PDF_CHANNEL);
export const handleAddressPDFRequests = () => {
  generateAddressPDFChannel.onReceive(
    (request: GenerateAddressPDFRendererRequest) =>
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

        const readAssetSync = (p) => fs.readFileSync(path.join(__dirname, p));

        let fontRegular;
        let fontMedium;

        if (currentLocale === 'ja-JP') {
          fontRegular = fontUnicode;
          fontMedium = fontUnicode;
        } else {
          fontRegular = fontRegularEn;
          fontMedium = fontMediumEn;
        }

        // Generate QR image for wallet address
        const qrCodeImage = qr.imageSync(address, {
          type: 'png',
          size: 10,
          ec_level: 'L',
          margin: 0,
        });

        try {
          const fontBufferMedium = readAssetSync(fontMedium);
          const fontBufferRegular = readAssetSync(fontRegular);
          const fontBufferMono = readAssetSync(fontMono);
          const fontBufferUnicode = readAssetSync(fontUnicode);
          let noteHeight = 0;

          if (note) {
            noteHeight = getHeightOfString(note, fontBufferRegular, 14) + 30;
          }

          const textColor = '#5e6066';
          const textColorRed = '#ea4c5b';
          const width = 640;
          const height = 450 + noteHeight;
          const doc = new PDFDocument({
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
          const writeStream = fs.createWriteStream(filePath);
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
