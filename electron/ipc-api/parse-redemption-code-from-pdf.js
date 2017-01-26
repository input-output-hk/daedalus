import { PDFExtract } from 'pdf.js-extract';
import { ipcMain } from 'electron';

const CHANNEL = 'parse-redemption-code-from-pdf';

export const PARSE_REDEMPTION_CODE = {
  REQUEST: CHANNEL,
  SUCCESS: `${CHANNEL}-success`,
  ERROR: `${CHANNEL}-error`,
};

export default () => {
  ipcMain.on(CHANNEL, (event, pdfPath) => {
    const sender = event.sender;
    try {
      const pdfExtract = new PDFExtract();
      pdfExtract.extract(pdfPath, {}, function (err, data) {
        if (error) sender.send(PARSE_REDEMPTION_CODE.ERROR, error);
        sender.send(PARSE_REDEMPTION_CODE.SUCCESS, data.pages[0].content[8].str);
      });
    } catch(error) {
      sender.send(PARSE_REDEMPTION_CODE.ERROR, error);
    }
  });
};
