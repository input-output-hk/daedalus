'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.downloadCsv = void 0;
const generateCsvChannel_1 = require('../ipc/generateCsvChannel');
const downloadCsv = async ({ fileContent, filePath }) => {
  await generateCsvChannel_1.generateCsvChannel.send({
    fileContent,
    filePath,
  });
};
exports.downloadCsv = downloadCsv;
//# sourceMappingURL=csvGenerator.js.map
