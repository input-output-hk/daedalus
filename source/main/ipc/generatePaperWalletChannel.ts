import fs from 'fs';
import path from 'path';
import PDFDocument from 'pdfkit';
import qr from 'qr-image';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { GENERATE_PAPER_WALLET_CHANNEL } from '../../common/ipc/api';
import type {
  GeneratePaperWalletMainResponse,
  GeneratePaperWalletRendererRequest,
} from '../../common/ipc/api';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../common/assets/pdf/paper-... Remove this comment to see the full error message
import paperWalletFontPath from '../../common/assets/pdf/paper-wallet-certificate-font.ttf';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../common/assets/pdf/paper-... Remove this comment to see the full error message
import paperWalletPage1Path from '../../common/assets/pdf/paper-wallet-certificate-page-1.png';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../common/assets/pdf/paper-... Remove this comment to see the full error message
import paperWalletPage1PathTestnet from '../../common/assets/pdf/paper-wallet-certificate-page-1-testnet.png';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../common/assets/pdf/paper-... Remove this comment to see the full error message
import paperWalletPage2Path from '../../common/assets/pdf/paper-wallet-certificate-page-2.png';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../common/assets/pdf/paper-... Remove this comment to see the full error message
import paperWalletPage2PathTestnet from '../../common/assets/pdf/paper-wallet-certificate-page-2-testnet.png';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../common/assets/pdf/paper-... Remove this comment to see the full error message
import paperWalletCertificateBgPath from '../../common/assets/pdf/paper-wallet-certificate-background.png';

export const generatePaperWalletChannel: // IpcChannel<Incoming, Outgoing>
MainIpcChannel<
  GeneratePaperWalletRendererRequest,
  GeneratePaperWalletMainResponse
> = new MainIpcChannel(GENERATE_PAPER_WALLET_CHANNEL);
export const handlePaperWalletRequests = () => {
  generatePaperWalletChannel.onReceive(
    (request: GeneratePaperWalletRendererRequest) =>
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

        const readAssetSync = (p) => fs.readFileSync(path.join(__dirname, p));

        // Generate QR image for wallet address
        const qrCodeImage = qr.imageSync(address, {
          type: 'png',
          size: 10,
          ec_level: 'L',
          margin: 0,
        });
        const textColor = '#3b5c9b';
        const width = 595.28;
        const height = 841.98;
        const doc = new PDFDocument({
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
          const fontBuffer = readAssetSync(paperWalletFontPath);
          doc.font(fontBuffer);
          // background images
          const backgroundUri = readAssetSync(paperWalletCertificateBgPath);
          doc.image(backgroundUri, 0, 0, {
            fit: [width, height],
          });
          // first page
          const page1Uri = readAssetSync(
            isMainnet ? paperWalletPage1Path : paperWalletPage1PathTestnet
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
            isMainnet ? paperWalletPage2Path : paperWalletPage2PathTestnet
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
        const writeStream = fs.createWriteStream(filePath);
        doc.pipe(writeStream);
        doc.end();
        writeStream.on('close', resolve);
        writeStream.on('error', reject);
      })
  );
};
