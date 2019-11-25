// @flow
import fs from 'fs';
import path from 'path';
import PDFDocument from 'pdfkit';
import qr from 'qr-image';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { GENERATE_ADDRESS_PDF_CHANNEL } from '../../common/ipc/api';
import type {
  GenerateAddressPDFRendererRequest,
  GenerateAddressPDFMainResponse,
} from '../../common/ipc/api';
import fontRegular from '../../common/assets/pdf/NotoSans-Medium.ttf';
import fontMono from '../../common/assets/pdf/SFMono-Light.ttf';

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
          title,
          creationDate,
          address,
          filePath,
          note,
          noteTitle,
          author,
        } = request;

        const readAssetSync = p => fs.readFileSync(path.join(__dirname, p));

        // Generate QR image for wallet address
        const qrCodeImage = qr.imageSync(address, {
          type: 'png',
          size: 10,
          ec_level: 'L',
          margin: 0,
        });
        const textColor = '#5e6066';
        const width = 640;
        const height = note ? 470 : 360;
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

        try {
          const fontBufferRegular = readAssetSync(fontRegular);
          const fontBufferMono = readAssetSync(fontMono);

          // Title
          doc
            .font(fontBufferRegular)
            .fontSize(18)
            .text(title.toUpperCase(), {
              align: 'center',
              characterSpacing: 2,
            });

          // Creation date
          doc.fontSize(14).text(creationDate.toUpperCase(), {
            align: 'center',
          });

          doc.moveDown();

          // QR Code
          doc.image(qrCodeImage, {
            fit: [width - 60, 192],
            align: 'center',
          });

          doc.moveDown();

          // Address
          doc
            .font(fontBufferMono)
            .fontSize(19)
            .text(address, {
              align: 'center',
              characterSpacing: 1.5,
            });

          if (note) {
            doc.moveDown();
            // Note title
            doc
              .font(fontBufferRegular)
              .fontSize(14)
              .text(noteTitle);

            // Note
            doc.font(fontBufferMono).text(note);
          }
        } catch (error) {
          reject(error);
        }

        // Write file to disk
        const writeStream = fs.createWriteStream(filePath);
        doc.pipe(writeStream);
        doc.end();
        writeStream.on('close', resolve);
        writeStream.on('error', reject);
      })
  );
};
