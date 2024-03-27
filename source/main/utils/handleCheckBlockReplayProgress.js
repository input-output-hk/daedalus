'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.handleCheckBlockReplayProgress = void 0;
const fs_1 = __importDefault(require('fs'));
const moment_1 = __importDefault(require('moment'));
const path_1 = __importDefault(require('path'));
const tail_1 = require('tail');
const get_block_sync_progress_1 = require('../ipc/get-block-sync-progress');
const cardano_node_types_1 = require('../../common/types/cardano-node.types');
const blockSyncProgressHelpers_1 = require('./blockSyncProgressHelpers');
const environment_1 = require('../environment');
const blockKeyword = 'Replayed block';
const validatingChunkKeyword = 'Validating chunk';
const validatedChunkKeyword = 'Validated chunk';
const ledgerKeyword = 'Pushing ledger state';
const progressKeywords = [
  blockKeyword,
  validatingChunkKeyword,
  validatedChunkKeyword,
  ledgerKeyword,
];
const keywordTypeMap = {
  [blockKeyword]: cardano_node_types_1.BlockSyncType.replayedBlock,
  [validatingChunkKeyword]: cardano_node_types_1.BlockSyncType.validatingChunk,
  [validatedChunkKeyword]: cardano_node_types_1.BlockSyncType.validatingChunk,
  [ledgerKeyword]: cardano_node_types_1.BlockSyncType.pushingLedger,
};
function containProgressKeywords(line) {
  return progressKeywords.some((keyword) => line.includes(keyword));
}
function getProgressType(line) {
  const key = progressKeywords.find((k) => line.includes(k));
  if (!key) {
    return null;
  }
  return keywordTypeMap[key];
}
const applicationStartDate = moment_1.default.utc();
const createHandleNewLogLine = (mainWindow) => {
  const progressReport = {
    [cardano_node_types_1.BlockSyncType.validatingChunk]: 0,
    [cardano_node_types_1.BlockSyncType.replayedBlock]: 0,
    [cardano_node_types_1.BlockSyncType.pushingLedger]: 0,
  };
  return (line) => {
    if (
      !(0, blockSyncProgressHelpers_1.isItFreshLog)(
        applicationStartDate,
        line
      ) ||
      !containProgressKeywords(line)
    ) {
      return;
    }
    const unparsedProgress = line.match(/Progress:([\s\d.,]+)%/)?.[1];
    const type = getProgressType(line);
    if (!unparsedProgress || !type) {
      return;
    }
    const progress = parseFloat(unparsedProgress);
    if (progressReport[type] !== progress) {
      progressReport[type] = progress;
      get_block_sync_progress_1.getBlockSyncProgressChannel.send(
        progressReport,
        mainWindow.webContents
      );
    }
  };
};
const watchLogFile = ({ logFilePath, mainWindow }) => {
  const tail = new tail_1.Tail(logFilePath, {
    // using fs.watchFile instead of fs.watch on Windows because of Node API issues:
    // https://github.com/nodejs/node/issues/36888
    // https://github.com/lucagrulla/node-tail/issues/137
    // https://nodejs.org/dist/latest-v14.x/docs/api/fs.html#fs_caveats
    useWatchFile: environment_1.environment.isWindows,
    fromBeginning: true,
  });
  const handleNewLogLine = createHandleNewLogLine(mainWindow);
  tail.on('line', handleNewLogLine);
};
const waitForLogFileToBeCreatedAndWatchIt = ({
  logFileName,
  logFileDirPath,
  mainWindow,
}) => {
  const watcher = fs_1.default.watch(logFileDirPath, {}, (eventName, file) => {
    if (eventName === 'rename' && logFileName === file) {
      watchLogFile({
        logFilePath: path_1.default.join(logFileDirPath, logFileName),
        mainWindow,
      });
      watcher.close();
    }
  });
};
const handleCheckBlockReplayProgress = (mainWindow, logsDirectoryPath) => {
  const logFileName = 'node.log';
  const logFileDirPath = `${logsDirectoryPath}/pub/`;
  const logFilePath = path_1.default.join(logFileDirPath, logFileName);
  if (!fs_1.default.existsSync(logFilePath)) {
    waitForLogFileToBeCreatedAndWatchIt({
      logFileDirPath,
      logFileName,
      mainWindow,
    });
  } else {
    watchLogFile({
      logFilePath,
      mainWindow,
    });
  }
};
exports.handleCheckBlockReplayProgress = handleCheckBlockReplayProgress;
//# sourceMappingURL=handleCheckBlockReplayProgress.js.map
