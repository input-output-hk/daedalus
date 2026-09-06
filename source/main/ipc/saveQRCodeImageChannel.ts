import fs from 'fs';
import qr from 'qr-image';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { GENERATE_QRCODE_CHANNEL } from '../../common/ipc/api';
import type {
  GenerateQRCodeRendererRequest,
  GenerateQRCodeMainResponse,
} from '../../common/ipc/api';

export const saveQRCodeImageChannel: // IpcChannel<Incoming, Outgoing>
MainIpcChannel<
  GenerateQRCodeRendererRequest,
  GenerateQRCodeMainResponse
> = new MainIpcChannel(GENERATE_QRCODE_CHANNEL);
export const saveQRCodeImageRequests = () => {
  saveQRCodeImageChannel.onReceive(
    (request: GenerateQRCodeRendererRequest) =>
      new Promise((resolve, reject) => {
        const { address, filePath } = request;

        try {
          const qrCodeImage = qr.image(address, {
            type: 'png',
            size: 10,
            ec_level: 'L',
          });
          const writeStream = fs.createWriteStream(filePath);
          qrCodeImage.pipe(writeStream);
          writeStream.on('close', resolve);
          writeStream.on('error', reject);
        } catch (error) {
          reject(error);
        }
      })
  );
};
