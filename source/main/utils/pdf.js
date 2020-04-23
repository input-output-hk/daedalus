import PDFDocument from 'pdfkit';

const doc = new PDFDocument();
export const getHeightOfString = (text, font, fontSize) => {
  doc.font(font).fontSize(fontSize);
  return doc.heightOfString(text);
};
