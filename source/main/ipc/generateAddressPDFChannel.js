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
import font from '../../common/assets/pdf/NotoSans-Medium.ttf';

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
          contentTitle,
          filePath,
          fileTitle,
          fileAuthor,
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
        const height = 441;
        const doc = new PDFDocument({
          size: [width, height],
          margins: {
            bottom: 20,
            left: 30,
            right: 30,
            top: 20,
          },
          info: {
            Title: fileTitle,
            Author: fileAuthor,
          },
        });
        try {
          // font family
          const fontBuffer = readAssetSync(font);
          doc.font(fontBuffer);
          doc.fillColor(textColor);

          // Title
          doc.fontSize(18).text(contentTitle, 119, 484);
          doc.image(qrCodeImage, width / 2 - 80 / 2, 180, { fit: [80, 80] });
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
