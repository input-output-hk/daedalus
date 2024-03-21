'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.generateFileMeta = void 0;
const generateFileMetaChannel_1 = require('../ipc/generateFileMetaChannel');
const generateFileMeta = async ({ filePath }) => {
  const fileMeta = await generateFileMetaChannel_1.generateFileMetaChannel.send(
    {
      filePath,
    }
  );
  return fileMeta;
};
exports.generateFileMeta = generateFileMeta;
//# sourceMappingURL=fileMetaGenerator.js.map
