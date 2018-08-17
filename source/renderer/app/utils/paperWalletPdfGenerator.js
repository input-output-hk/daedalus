// @flow
import PDFDocument from 'pdfkit';
import qr from 'qr-image';
import { defineMessages } from 'react-intl';
import fs from 'fs';
import paperWalletFontPath from '../assets/pdf/paper-wallet-certificate-font.ttf';
import paperWalletPage1Path from '../assets/pdf/paper-wallet-certificate-page-1.png';
import paperWalletPage2Path from '../assets/pdf/paper-wallet-certificate-page-2.png';
import paperWalletCertificateBgPath from '../assets/pdf/paper-wallet-certificate-background.png';
import environment from '../../../common/environment';
import { loadAssetChannel } from '../ipc/loadAsset';

const messages = defineMessages({
  walletAddressLabel: {
    id: 'paper.wallet.pdf.walletAddress.label',
    defaultMessage: '!!!Wallet address',
    description: 'Paper wallet pdf "Wallet address" label.'
  },
  recoveryPhraseLabel: {
    id: 'paper.wallet.pdf.recoveryPhrase.label',
    defaultMessage: '!!!Paper wallet recovery phrase',
    description: 'Paper wallet pdf "Paper wallet recovery phrase" label.'
  },
  passwordLabel: {
    id: 'paper.wallet.pdf.password.label',
    defaultMessage: '!!!Password',
    description: 'Paper wallet pdf "Password" label.'
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

type DownloadPaperWalletCertificateParams = {
  address: string,
  filePath: string,
  mnemonics: Array<string>,
  intl: Object,
};

export const downloadPaperWalletCertificate = async (
  { address, mnemonics, intl, filePath }: DownloadPaperWalletCertificateParams
) => {
  const { getBuildLabel } = environment;
  const qrCodeImage = qr.imageSync(address, { type: 'png', size: 10, ec_level: 'L', margin: 0 });
  const textColor = '#3b5c9b';

  const printMnemonic = (index) => `${index + 1}. ${mnemonics[index]}`;

  const loadAssetFromPath = async (path) => {
    const asset = await loadAssetChannel.send({ fileName: path });
    return asset;
  };

  const loadFontBufferFromPath = async (path) => {
    const asset = await loadAssetFromPath(path);
    return Buffer.from(asset, 'base64');
  };

  const loadImageUriFromPath = async (path) => {
    const asset = await loadAssetFromPath(path);
    return `data:image/png;base64,${asset}`;
  };

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
      Title: intl.formatMessage(messages.infoTitle),
      Author: intl.formatMessage(messages.infoAuthor),
    }
  });

  /* eslint-disable max-len */
  // font family
  const fontBuffer = await loadFontBufferFromPath(paperWalletFontPath);
  doc.font(fontBuffer);

  // background images
  const backgroundUri = await loadImageUriFromPath(paperWalletCertificateBgPath);
  doc.image(backgroundUri, 0, 0, { fit: [width, height] });

  // first page
  const page1Uri = await loadImageUriFromPath(paperWalletPage1Path);
  doc.image(page1Uri, 0, 0, { fit: [width, height] });
  doc.rotate(180, { origin: [width / 2, height / 2] });
  doc.fillColor(textColor);
  doc.fontSize(10).text(intl.formatMessage(messages.walletAddressLabel), 0, 160, { width: 595, align: 'center' });
  doc.image(qrCodeImage, (width / 2) - 80 / 2, 180, { fit: [80, 80] });
  doc.fontSize(8).text(address, (width - 250) / 2, 274, { width: 250, align: 'center', lineGap: 2 });

  // revert document rotation
  doc.rotate(-180, { origin: [width / 2, height / 2] });

  // second page
  doc.addPage();
  const page2Uri = await loadImageUriFromPath(paperWalletPage2Path);
  doc.image(page2Uri, 0, 0, { fit: [width, height] });
  doc.rotate(180, { origin: [width / 2, height / 2] });
  doc.fillColor(textColor);
  doc.fontSize(10).text(intl.formatMessage(messages.recoveryPhraseLabel), 0, 535, { width: 595, align: 'center' });

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

  doc.fontSize(7).text(getBuildLabel(), (width - 270) / 2, 705, { width: 270, align: 'left' });
  doc.rotate(-180, { origin: [width / 2, height / 2] });
  /* eslint-enable max-len */

  doc.pipe(fs.createWriteStream(filePath));
  doc.end();
};
