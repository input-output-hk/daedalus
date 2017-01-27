import { PDFExtract } from 'pdf.js-extract';
import { ipcMain } from 'electron';

const CHANNEL_NAME = 'parse-redemption-code-from-pdf';

export const PARSE_REDEMPTION_CODE = {
  REQUEST: CHANNEL_NAME,
  SUCCESS: `${CHANNEL_NAME}-success`,
  ERROR: `${CHANNEL_NAME}-error`,
};

export default () => {
  ipcMain.on(PARSE_REDEMPTION_CODE.REQUEST, (event, pdfPath) => {
    const sender = event.sender;
    try {
      const pdfExtract = new PDFExtract();
      pdfExtract.extract(pdfPath, {}, function (error, data) {
        if (error) sender.send(PARSE_REDEMPTION_CODE.ERROR, error);
        sender.send(PARSE_REDEMPTION_CODE.SUCCESS, data.pages[0].content[8].str);
      });
    } catch(error) {
      sender.send(PARSE_REDEMPTION_CODE.ERROR, error);
    }
  });
};
