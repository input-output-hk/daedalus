// @flow
import PDFDocument from 'pdfkit';
import SVGtoPDF from 'svg-to-pdfkit';
import qr from 'qr-image';
import { defineMessages } from 'react-intl';
import fs from 'fs';
import paperWalletPage1 from '../assets/pdf/paper-wallet-certificate-page-1.inline.svg';
import paperWalletPage2 from '../assets/pdf/paper-wallet-certificate-page-2.inline.svg';
import paperWalletCertificateBg from '../assets/pdf/paper-wallet-certificate-background.png';
import environment from '../../../common/environment';

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
  releaseVersion: {
    id: 'paper.wallet.pdf.release.version',
    defaultMessage: '!!!0.8.2',
    description: 'Label for "App Release Version"',
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
  onSuccess?: Function,
  onError?: Function,
};

export const downloadPaperWalletCertificate = (
  { address, mnemonics, intl, onSuccess, onError, filePath }: DownloadPaperWalletCertificateParams
) => {
  const daedalusInfo =
    `Daedalus ${intl.formatMessage(messages.releaseVersion)}#${environment.build}`;
  const qrCodeImage = qr.imageSync(address, { type: 'png', size: 10, ec_level: 'H', margin: 0 });
  const textColor = '#3b5c9b';

  const printMnemonic = (index) => `${index + 1}. ${mnemonics[index]}`;

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
  // background images
  doc.image(paperWalletCertificateBg, 0, 4, { fit: [width, height] });

  // first page
  SVGtoPDF(doc, paperWalletPage1, 0, 0, { precision: 10 });
  doc.rotate(180, { origin: [width / 2, height / 2] });
  doc.fillColor(textColor);
  doc.fontSize(10).text(intl.formatMessage(messages.walletAddressLabel), 0, 160, { width: 595, align: 'center' });
  doc.image(qrCodeImage, (width / 2) - 80 / 2, 180, { fit: [80, 80] });
  doc.fontSize(8).text(address, (width - 250) / 2, 274, { width: 250, align: 'center', lineGap: 2 });

  // revert document rotation
  doc.rotate(-180, { origin: [width / 2, height / 2] });

  // second page
  doc.addPage();
  SVGtoPDF(doc, paperWalletPage2, 0, 0);
  doc.rotate(180, { origin: [width / 2, height / 2] });
  doc.fillColor(textColor);
  doc.fontSize(10).text(intl.formatMessage(messages.recoveryPhraseLabel), 0, 535, { width: 595, align: 'center' });

  // mnemonics
  doc.fontSize(8);
  doc.text(printMnemonic(0), 170, 560);
  doc.text(printMnemonic(1), 236, 560);
  doc.text(printMnemonic(2), 302, 560);
  doc.text(printMnemonic(3), 370, 560);

  doc.text(printMnemonic(4), 170, 578);
  doc.text(printMnemonic(5), 236, 578);
  doc.text(printMnemonic(6), 302, 578);
  doc.text(printMnemonic(7), 370, 578);

  doc.text(printMnemonic(8), 170, 596);
  doc.text(printMnemonic(9), 236, 596);
  doc.text(printMnemonic(10), 302, 596);
  doc.text(printMnemonic(11), 370, 596);

  doc.fontSize(7).text(daedalusInfo, (width - 270) / 2, 705, { width: 270, align: 'left' });
  doc.rotate(-180, { origin: [width / 2, height / 2] });
  /* eslint-enable max-len */

  try {
    doc.pipe(fs.createWriteStream(filePath));
    doc.end();
    if (onSuccess) onSuccess();
  } catch (e) {
    if (onError) onError(e);
    throw e;
  }
};
