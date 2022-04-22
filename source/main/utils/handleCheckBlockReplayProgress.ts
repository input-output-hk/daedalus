import { BrowserWindow } from 'electron';
import fs from 'fs';
import moment from 'moment';
import path from 'path';
import { Tail } from 'tail';
import debounce from 'lodash/debounce';
import { getBlockSyncProgressChannel } from '../ipc/get-block-sync-progress';
import type { GetBlockSyncProgressType } from '../../common/ipc/api';
import { BlockSyncType } from '../../common/types/cardano-node.types';
import { isItFreshLog } from './blockSyncProgressHelpers';
import { environment } from '../environment';

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

const keywordTypeMap: Record<string, GetBlockSyncProgressType> = {
  [blockKeyword]: BlockSyncType.replayedBlock,
  [validatingChunkKeyword]: BlockSyncType.validatingChunk,
  [validatedChunkKeyword]: BlockSyncType.validatingChunk,
  [ledgerKeyword]: BlockSyncType.pushingLedger,
};

function containProgressKeywords(line: string) {
  return progressKeywords.some((keyword) => line.includes(keyword));
}

function getProgressType(line: string): GetBlockSyncProgressType | null {
  const key = progressKeywords.find((k) => line.includes(k));

  if (!key) {
    return null;
  }

  return keywordTypeMap[key];
}

const applicationStartDate = moment.utc();

const createHandleNewLine = (mainWindow: BrowserWindow) =>
  debounce((line: string) => {
    if (
      !isItFreshLog(applicationStartDate, line) ||
      !containProgressKeywords(line)
    ) {
      return;
    }

    const percentage = line.match(/Progress:([\s\d.,]+)%/)?.[1];
    const progressType = getProgressType(line);
    if (!percentage || !progressType) {
      return;
    }
    const finalProgressPercentage = parseFloat(percentage);

    getBlockSyncProgressChannel.send(
      { progress: finalProgressPercentage, type: progressType },
      mainWindow.webContents
    );
  }, 1000);

export const handleCheckBlockReplayProgress = (
  mainWindow: BrowserWindow,
  logsDirectoryPath: string
) => {
  const filename = 'node.log';
  const logFilePath = `${logsDirectoryPath}/pub/`;
  const filePath = path.join(logFilePath, filename);
  if (!fs.existsSync(filePath)) return;

  const tail = new Tail(filePath, {
    // using fs.watchFile instead of fs.watch on Windows because of Node API issues:
    // https://github.com/nodejs/node/issues/36888
    // https://github.com/lucagrulla/node-tail/issues/137
    // https://nodejs.org/dist/latest-v14.x/docs/api/fs.html#fs_caveats
    useWatchFile: environment.isWindows,
  });

  const handleNewLine = createHandleNewLine(mainWindow);
  tail.on('line', handleNewLine);
};
