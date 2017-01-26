import { PDFExtract } from 'pdf.js-extract';
import { ipcMain } from 'electron';

export default () => {
  ipcMain.on('extract-redemption-code-from-pdf', (event, pdfPath) => {
    const pdfExtract = new PDFExtract();
    pdfExtract.extract(pdfPath, {}, function (err, data) {
      if (err) return console.log(err);
      event.returnValue = data.pages[0].content[8].str;
    });
  });
};
