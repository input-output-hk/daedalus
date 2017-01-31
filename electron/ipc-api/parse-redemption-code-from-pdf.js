import { PDFExtract } from 'pdf.js-extract';
import { ipcMain } from 'electron';
import fs from 'fs';
import decrypt from './lib/decrypt';

const CHANNEL_NAME = 'parse-redemption-code-from-pdf';

export const PARSE_REDEMPTION_CODE = {
  REQUEST: CHANNEL_NAME,
  SUCCESS: `${CHANNEL_NAME}-success`,
  ERROR: `${CHANNEL_NAME}-error`,
};

export default () => {
  ipcMain.on(PARSE_REDEMPTION_CODE.REQUEST, (event, filePath, passPhrase) => {
    const sender = event.sender;
    let pdfPath = null;
    let isTemporaryDecryptedPdf = false;
    // If pass phrase is given assume that it's an encrypted certificate
    if (passPhrase) {
      try {
        // Decrypt the file
        const encryptedFile = fs.readFileSync(filePath);
        const decryptedFile = decrypt(passPhrase, encryptedFile);
        // Write it to disk temporarily (so pdf extract can work with it)
        pdfPath = `${filePath}.pdf`;
        fs.writeFileSync(pdfPath, decryptedFile);
        isTemporaryDecryptedPdf = true;
      } catch (error) {
        sender.send(PARSE_REDEMPTION_CODE.ERROR, error.message);
      }
    } else {
      pdfPath = filePath;
    }
    // Extract redemption code from certificate PDF
    try {
      const pdfExtract = new PDFExtract();
      pdfExtract.extract(pdfPath, {}, function (error, data) {
        if (error) sender.send(PARSE_REDEMPTION_CODE.ERROR, error);
        sender.send(PARSE_REDEMPTION_CODE.SUCCESS, data.pages[0].content[8].str);
        try {
          // Remove the temporary, decrypted PDF from disk
          if (isTemporaryDecryptedPdf) fs.unlinkSync(pdfPath);
        } catch (error) {
          sender.send(PARSE_REDEMPTION_CODE.ERROR, error.message);
        }
      });
    } catch(error) {
      sender.send(PARSE_REDEMPTION_CODE.ERROR, error.message);
      // Remove the temporary, decrypted PDF from disk
      if (isTemporaryDecryptedPdf) fs.unlinkSync(pdfPath);
    }
  });
};
