'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.getHeightOfString = void 0;
const pdfkit_1 = __importDefault(require('pdfkit'));
const doc = new pdfkit_1.default();
const getHeightOfString = (text, font, fontSize) => {
  doc.font(font).fontSize(fontSize);
  return doc.heightOfString(text);
};
exports.getHeightOfString = getHeightOfString;
//# sourceMappingURL=pdf.js.map
