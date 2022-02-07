import fs from 'fs';
import path from 'path';
import PDFDocument from 'pdfkit';
import qr from 'qr-image';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { GENERATE_VOTING_PDF_CHANNEL } from '../../common/ipc/api';
import type {
  GenerateVotingPDFRendererRequest,
  GenerateVotingPDFMainResponse,
} from '../../common/ipc/api';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../common/assets/pdf/NotoSa... Remove this comment to see the full error message
import fontRegularEn from '../../common/assets/pdf/NotoSans-Regular.ttf';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../common/assets/pdf/NotoSa... Remove this comment to see the full error message
import fontMediumEn from '../../common/assets/pdf/NotoSans-Medium.ttf';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../common/assets/pdf/arial-... Remove this comment to see the full error message
import fontUnicode from '../../common/assets/pdf/arial-unicode.ttf';

export const generateVotingPDFChannel: // IpcChannel<Incoming, Outgoing>
MainIpcChannel<
  GenerateVotingPDFRendererRequest,
  GenerateVotingPDFMainResponse
> = new MainIpcChannel(GENERATE_VOTING_PDF_CHANNEL);
export const handleVotingPDFRequests = () => {
  generateVotingPDFChannel.onReceive(
    (request: GenerateVotingPDFRendererRequest) =>
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

        // Generate QR image for wallet voting
        const qrCodeImage = qr.imageSync(qrCode, {
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
          const doc = new PDFDocument({
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
