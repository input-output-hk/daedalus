import { PDFExtract } from 'pdf.js-extract';
import { ipcMain } from 'electron';
import fs from 'fs';
import Log from 'electron-log';
import { decryptRegularVend, decryptForceVend } from '../../lib/decrypt';

const CHANNEL_NAME = 'parse-redemption-code-from-pdf';

export const PARSE_REDEMPTION_CODE = {
  REQUEST: CHANNEL_NAME,
  SUCCESS: `${CHANNEL_NAME}-success`,
  ERROR: `${CHANNEL_NAME}-error`,
  INVALID_CERTIFICATE_ERROR: 'invalid certificate',
};

export default () => {
  ipcMain.on(PARSE_REDEMPTION_CODE.REQUEST, (event, filePath, decryptionKey, redemptionType) => {
    const sender = event.sender;
    let pdfPath = null;
    let isTemporaryDecryptedPdf = false;
    // If pass phrase is given assume that it's an encrypted certificate
    if (decryptionKey) {
      try {
        // Decrypt the file
        const encryptedFile = fs.readFileSync(filePath);
        const decryptedFile = redemptionType === 'regular' ?
          decryptRegularVend(decryptionKey, encryptedFile) :
          decryptForceVend(decryptionKey, encryptedFile);
        // Write it to disk temporarily (so pdf extract can work with it)
        pdfPath = `${filePath}.pdf`;
        fs.writeFileSync(pdfPath, decryptedFile);
        isTemporaryDecryptedPdf = true;
      } catch (error) {
        Log.warn('ERROR!', error);
        sender.send(PARSE_REDEMPTION_CODE.ERROR, error.message);
      }
    } else {
      pdfPath = filePath;
    }
    // Extract redemption code from certificate PDF
    try {
      const pdfExtract = new PDFExtract();
      pdfExtract.extract(pdfPath, {}, (error, data) => {
        if (error) return sender.send(PARSE_REDEMPTION_CODE.ERROR, error);
        try {
          const redemptionKeyLabel = data.pages[0].content[9].str;
          if (redemptionKeyLabel !== 'REDEMPTION KEY' && redemptionKeyLabel !== '—————— REDEMPTION KEY ——————') {
            return sender.send(
              PARSE_REDEMPTION_CODE.ERROR,
              PARSE_REDEMPTION_CODE.INVALID_CERTIFICATE_ERROR
            );
          }
          sender.send(PARSE_REDEMPTION_CODE.SUCCESS, data.pages[0].content[8].str);
          // Remove the temporary, decrypted PDF from disk
          if (isTemporaryDecryptedPdf) {
            try { fs.unlinkSync(pdfPath); } catch (e) {} // eslint-disable-line
          }
        } catch (exception) {
          sender.send(PARSE_REDEMPTION_CODE.ERROR, exception.message);
        }
      });
    } catch (error) {
      sender.send(PARSE_REDEMPTION_CODE.ERROR, error.message);
      // Remove the temporary, decrypted PDF from disk
      if (isTemporaryDecryptedPdf) {
        try { fs.unlinkSync(pdfPath); } catch (e) {} // eslint-disable-line
      }
    }
  });
};
