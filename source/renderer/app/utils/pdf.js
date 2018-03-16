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
  shieldedRecoveryPhraseLabel: {
    id: 'paper.wallet.pdf.shieldedRecoveryPhrase.label',
    defaultMessage: '!!!Shielded recovery phrase',
    description: 'Paper wallet pdf "Shielded recovery phrase" label.'
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
});

// TODO: WIP code, cleanup, rename labels, rename this file...

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
  const qrCodeImage = qr.imageSync(address, { type: 'png', size: 10, ec_level: 'H', margin: 0 });
  const titleColor = '#3b5c9b';
  const defautlColor = 'black';

  const width = 595.28;
  const height = 841.98;
  const doc = new PDFDocument({
    size: [width, height],
    margins: {
      top: 0,
      bottom: 0,
      left: 0,
      right: 0,
    },
    info: {
      Title: 'Daedalus - Paper wallet certificate',
      Author: 'Daedalus wallet',
    }
  });

  // background images
  doc.image(paperWalletCertificateBg, 0, 4, { fit: [width, height] });
  SVGtoPDF(doc, paperWalletPage1, 0, 0, { precision: 10 });

  // rotation of the document to print rotated content
  doc.rotate(180, { origin: [width / 2, height / 2] });

  // page one content
  doc.fontSize(10).fillColor(titleColor);
  doc.text(intl.formatMessage(messages.walletAddressLabel), 0, 160, { width: 595, align: 'center' });
  doc.fillColor(defautlColor);
  doc.image(qrCodeImage, (width / 2) - 80 / 2, 180, { fit: [80, 80] });
  doc.text(address, (width - 250) / 2, 274, { width: 250, align: 'center' });

  // rotation of the document back to normal after the rotated content has been printed
  doc.rotate(-180, { origin: [width / 2, height / 2] });

  // second page
  doc.addPage();

  // same deal as first page
  SVGtoPDF(doc, paperWalletPage2, 0, 0);
  doc.rotate(180, { origin: [width / 2, height / 2] });
  doc.fillColor(titleColor);
  doc.text(intl.formatMessage(messages.shieldedRecoveryPhraseLabel), 0, 550, { width: 595, align: 'center' });
  doc.fillColor(defautlColor);
  doc.fontSize(14).text(mnemonics.join('   '), (width - 250) / 2, 570, { width: 250, align: 'center' });
  doc.fontSize(10).fillColor(titleColor);
  doc.text(intl.formatMessage(messages.passwordLabel), 0, 655, { width: 595, align: 'center' });
  doc.fontSize(8).fillColor(defautlColor);
  doc.text(`Daedalus ${intl.formatMessage(messages.releaseVersion)}#${environment.build}`, 0, 695, { width: 595, align: 'center' });
  doc.rotate(-180, { origin: [width / 2, height / 2] });

  try {
    doc.pipe(fs.createWriteStream(filePath));
    doc.end();
    if (onSuccess) onSuccess();
  } catch (e) {
    if (onError) onError(e);
    throw e;
  }

};
