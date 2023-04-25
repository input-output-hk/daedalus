import { BrowserWindow } from 'electron';
import fs from 'fs';
import moment from 'moment';
import path from 'path';
import { Tail } from 'tail';
import { getBlockSyncProgressChannel } from '../ipc/get-block-sync-progress';
import {
  BlockSyncProgress,
  BlockSyncType,
} from '../../common/types/cardano-node.types';
import { isItFreshLog } from './blockSyncProgressHelpers';
import { environment } from '../environment';
import { logger } from './logging';

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

const keywordTypeMap: Record<string, BlockSyncType> = {
  [blockKeyword]: BlockSyncType.replayedBlock,
  [validatingChunkKeyword]: BlockSyncType.validatingChunk,
  [validatedChunkKeyword]: BlockSyncType.validatingChunk,
  [ledgerKeyword]: BlockSyncType.pushingLedger,
};

function containProgressKeywords(line: string) {
  return progressKeywords.some((keyword) => line.includes(keyword));
}

function getProgressType(line: string): BlockSyncType | null {
  const key = progressKeywords.find((k) => line.includes(k));

  if (!key) {
    return null;
  }

  return keywordTypeMap[key];
}

const applicationStartDate = moment.utc();

const createHandleNewLogLine = (mainWindow: BrowserWindow) => {
  const progressReport: BlockSyncProgress = {
    [BlockSyncType.validatingChunk]: 0,
    [BlockSyncType.replayedBlock]: 0,
    [BlockSyncType.pushingLedger]: 0,
  };

  return (line: string) => {
    if (
      !isItFreshLog(applicationStartDate, line) ||
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
      getBlockSyncProgressChannel.send(progressReport, mainWindow.webContents);
    }
  };
};

const watchLogFile = ({
  logFilePath,
  mainWindow,
}: {
  logFilePath: string;
  mainWindow: BrowserWindow;
}) => {
  const tail = new Tail(logFilePath, {
    // using fs.watchFile instead of fs.watch on Windows because of Node API issues:
    // https://github.com/nodejs/node/issues/36888
    // https://github.com/lucagrulla/node-tail/issues/137
    // https://nodejs.org/dist/latest-v14.x/docs/api/fs.html#fs_caveats
    useWatchFile: environment.isWindows,
    fromBeginning: true,
  });

  const handleNewLogLine = createHandleNewLogLine(mainWindow);
  tail.on('line', handleNewLogLine);
};

const waitForLogFileToBeCreatedAndWatchIt = ({
  logFileName,
  logFileDirPath,
  mainWindow,
}: {
  logFileName: string;
  logFileDirPath: string;
  mainWindow: BrowserWindow;
}) => {
  const watcher = fs.watch(logFileDirPath, {}, (eventName, file) => {
    if (eventName === 'rename' && logFileName === file) {
      watchLogFile({
        logFilePath: path.join(logFileDirPath, logFileName),
        mainWindow,
      });
      watcher.close();
    }
  });
};

export const handleCheckBlockReplayProgress = (
  mainWindow: BrowserWindow,
  logsDirectoryPath: string
) => {
  const logFileName = 'node.log';
  const logFileDirPath = `${logsDirectoryPath}/pub/`;
  const logFilePath = path.join(logFileDirPath, logFileName);

  if (!fs.existsSync(logFilePath)) {
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
