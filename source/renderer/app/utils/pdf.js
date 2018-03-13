// @flow
import * as pdfMake from 'pdfmake/build/pdfmake';
import * as pdfFonts from 'pdfmake/build/vfs_fonts';
import { defineMessages } from 'react-intl';
import fs from 'fs';
import paperWalletPage1 from '../assets/pdf/paper-wallet-certificate-page-1.png';
import paperWalletPage2 from '../assets/pdf/paper-wallet-certificate-page-2.png';
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

// Assign pdfFonts to pdfMaker
Object.assign(pdfMake, pdfFonts.pdfMake);
Object.assign(pdfMake.default, pdfFonts.default.pdfMake);

// define your function for generating rotated text
const writeRotatedText = ({ text, width, height }) => {
  const verticalSpacing = 3;
  const fontSize = height - verticalSpacing;
  const qualityMultiplier = 4;
  // ^^ qualityMultiplier is used to generate HQ canvas and then fit it to A4 page width
  const canvas = document.createElement('canvas');
  canvas.width = width * qualityMultiplier;
  canvas.height = height * qualityMultiplier;
  const ctx = canvas.getContext('2d');
  ctx.font = `${fontSize * qualityMultiplier}pt Arial`;
  ctx.save();
  ctx.translate(canvas.width, 0);
  ctx.scale(-1, -1);
  ctx.textAlign = 'center';
  ctx.fillStyle = '#3b5c9b';
  ctx.fillText(text, canvas.width / 2, -verticalSpacing * qualityMultiplier);
  ctx.restore();
  return canvas.toDataURL();
};

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
  // Since there is no auto-wrapping we need to split wallet address into 2 lines manually
  const middleIndex = Math.round(address.length / 2);
  const addressLine1 = address.slice(0, middleIndex);
  const addressLine2 = address.slice(middleIndex);

  const docDefinition = {
    content: [
      // 1st page - Public key
      { // Page background
        image: paperWalletPage1,
        absolutePosition: { x: 0, y: 0 },
        width: 595.28,
        height: 841.89,
      },
      { // Wallet address
        image: writeRotatedText({
          text: intl.formatMessage(messages.walletAddressLabel),
          width: 500,
          height: 10,
        }),
        fit: [500, 10],
        alignment: 'center',
        absolutePosition: { x: 0, y: 620 },
      },
      { // Wallet address QR code
        qr: address,
        alignment: 'center',
        background: '#f8fbfd',
        fit: 80,
        foreground: '#3b5c9b',
        absolutePosition: { x: 0, y: 540 },
      },
      { // Wallet address - line 1
        image: writeRotatedText({ text: addressLine1, width: 500, height: 8 }),
        fit: [500, 8],
        alignment: 'center',
        absolutePosition: { x: 0, y: 525 },
      },
      { // Wallet address - line 2
        image: writeRotatedText({ text: addressLine2, width: 500, height: 8 }),
        fit: [500, 8],
        alignment: 'center',
        absolutePosition: { x: 0, y: 515 },
      },
      { // Daedalus version and build
        image: writeRotatedText({
          text: `Daedalus ${intl.formatMessage(messages.releaseVersion)}#${environment.build}`,
          width: 100,
          height: 8,
        }),
        fit: [100, 8],
        absolutePosition: { x: 343, y: 493 },
      },
      // 2nd page - Private key
      { // Page background
        image: paperWalletPage2,
        absolutePosition: { x: 0, y: 0 },
        width: 595.28,
        height: 841.89,
        pageBreak: 'before',
      },
      { // Shielded recovery phrase
        image: writeRotatedText({
          text: intl.formatMessage(messages.shieldedRecoveryPhraseLabel),
          width: 500,
          height: 10,
        }),
        fit: [500, 10],
        alignment: 'center',
        absolutePosition: { x: 0, y: 270 },
      },
      { // Shielded recovery phrase - word 1
        image: writeRotatedText({ text: mnemonics[0], width: 50, height: 8 }),
        fit: [50, 8],
        absolutePosition: { x: 375, y: 245 },
      },
      { // Shielded recovery phrase - word 2
        image: writeRotatedText({ text: mnemonics[1], width: 50, height: 8 }),
        fit: [50, 8],
        absolutePosition: { x: 325, y: 245 },
      },
      { // Shielded recovery phrase - word 3
        image: writeRotatedText({ text: mnemonics[2], width: 50, height: 8 }),
        fit: [50, 8],
        absolutePosition: { x: 275, y: 245 },
      },
      { // Shielded recovery phrase - word 4
        image: writeRotatedText({ text: mnemonics[3], width: 50, height: 8 }),
        fit: [50, 8],
        absolutePosition: { x: 225, y: 245 },
      },
      { // Shielded recovery phrase - word 5
        image: writeRotatedText({ text: mnemonics[4], width: 50, height: 8 }),
        fit: [50, 8],
        absolutePosition: { x: 175, y: 245 },
      },
      { // Shielded recovery phrase - word 6
        image: writeRotatedText({ text: mnemonics[5], width: 50, height: 8 }),
        fit: [50, 8],
        absolutePosition: { x: 375, y: 230 },
      },
      { // Shielded recovery phrase - word 7
        image: writeRotatedText({ text: mnemonics[6], width: 50, height: 8 }),
        fit: [50, 8],
        absolutePosition: { x: 325, y: 230 },
      },
      { // Shielded recovery phrase - word 8
        image: writeRotatedText({ text: mnemonics[7], width: 50, height: 8 }),
        fit: [50, 8],
        absolutePosition: { x: 275, y: 230 },
      },
      { // Shielded recovery phrase - word 9
        image: writeRotatedText({ text: mnemonics[8], width: 50, height: 8 }),
        fit: [50, 8],
        absolutePosition: { x: 225, y: 230 },
      },
      { // Shielded recovery phrase - word 10
        image: writeRotatedText({ text: mnemonics[9], width: 50, height: 8 }),
        fit: [50, 8],
        absolutePosition: { x: 175, y: 230 },
      },
      { // Shielded recovery phrase - word 11
        image: writeRotatedText({ text: mnemonics[10], width: 50, height: 8 }),
        fit: [50, 8],
        absolutePosition: { x: 375, y: 215 },
      },
      { // Shielded recovery phrase - word 12
        image: writeRotatedText({ text: mnemonics[11], width: 50, height: 8 }),
        fit: [50, 8],
        absolutePosition: { x: 325, y: 215 },
      },
      { // Shielded recovery phrase - word 13
        image: writeRotatedText({ text: mnemonics[12], width: 50, height: 8 }),
        fit: [50, 8],
        absolutePosition: { x: 275, y: 215 },
      },
      { // Shielded recovery phrase - word 14
        image: writeRotatedText({ text: mnemonics[13], width: 50, height: 8 }),
        fit: [50, 8],
        absolutePosition: { x: 225, y: 215 },
      },
      { // Shielded recovery phrase - word 15
        image: writeRotatedText({ text: mnemonics[14], width: 50, height: 8 }),
        fit: [50, 8],
        absolutePosition: { x: 175, y: 215 },
      },
      { // Password
        image: writeRotatedText({
          text: intl.formatMessage(messages.passwordLabel),
          width: 500,
          height: 10,
        }),
        fit: [500, 10],
        alignment: 'center',
        absolutePosition: { x: 0, y: 180 },
      },
    ],
    pageMargins: [0, 0],
    pageSize: 'A4',
  };

  // create file stream from buffer and save to provided path
  pdfMake.createPdf(docDefinition).getBuffer((buffer) => {
    try {
      fs.writeFileSync(filePath, buffer);
      if (onSuccess) onSuccess();
    } catch (e) {
      if (onError) onError(e);
      throw e;
    }
  });
};
