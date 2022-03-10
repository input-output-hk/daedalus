import { BrowserWindow } from 'electron';
import fs from 'fs';
import moment from 'moment';
import path from 'path';
import { Tail } from 'tail';
import { getBlockSyncProgressChannel } from '../ipc/get-block-sync-progress';
import type { GetBlockSyncProgressType } from '../../common/ipc/api';
import { BlockSyncType } from '../../common/types/cardano-node.types';
import { isItFreshLog } from './blockSyncProgressHelpers';

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

export const handleCheckBlockReplayProgress = (
  mainWindow: BrowserWindow,
  logsDirectoryPath: string
) => {
  const filename = 'node.log';
  const logFilePath = `${logsDirectoryPath}/pub/`;
  const filePath = path.join(logFilePath, filename);
  if (!fs.existsSync(filePath)) return;

  const tail = new Tail(filePath);

  tail.on('line', (line) => {
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
    // Send result to renderer process (NetworkStatusStore)
    getBlockSyncProgressChannel.send(
      { progress: finalProgressPercentage, type: progressType },
      mainWindow.webContents
    );
  });
};
