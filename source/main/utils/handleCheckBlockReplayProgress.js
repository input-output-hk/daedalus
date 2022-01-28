// @flow
import { BrowserWindow } from 'electron';
import fs from 'fs';
import readline from 'readline';
import path from 'path';
import { getBlockSyncProgressChannel } from '../ipc/get-block-sync-progress';
import type { GetBlockSyncProgressType } from '../../common/ipc/api';
import { BLOCK_REPLAY_PROGRESS_CHECK_INTERVAL } from '../config';

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

type KeywordTypeMap = {
  [name: string]: GetBlockSyncProgressType,
};

const keywordTypeMap: KeywordTypeMap = {
  [blockKeyword]: 'replayedBlock',
  [validatingChunkKeyword]: 'validatingChunk',
  [validatedChunkKeyword]: 'validatingChunk',
  [ledgerKeyword]: 'pushingLedger',
};

function containProgressKeywords(line: string) {
  return progressKeywords.some((keyword) => line.includes(keyword));
}

function getProgressType(line: string): ?GetBlockSyncProgressType {
  const key = progressKeywords.find((k) => line.includes(k));

  if (!key) {
    return null;
  }

  return keywordTypeMap[key];
}

export const handleCheckBlockReplayProgress = (
  mainWindow: BrowserWindow,
  logsDirectoryPath: string
) => {
  const checkBlockReplayProgress = async () => {
    const filename = 'node.log';
    const logFilePath = `${logsDirectoryPath}/pub/`;
    const filePath = path.join(logFilePath, filename);
    if (!fs.existsSync(filePath)) return;

    const fileStream = fs.createReadStream(filePath);
    const rl = readline.createInterface({ input: fileStream });
    const progress = [];
    for await (const line of rl) {
      if (containProgressKeywords(line)) {
        progress.push(line);
      }
    }
    if (!progress.length) return;

    const finalProgress = progress.slice(-1).pop();
    const percentage = finalProgress.match(/Progress:([\s\d.,]+)%/)?.[1];
    const progressType = getProgressType(finalProgress);
    if (!percentage || !progressType) {
      return;
    }
    const finalProgressPercentage = parseFloat(percentage);

    // Send result to renderer process (NetworkStatusStore)
    getBlockSyncProgressChannel.send(
      { progress: finalProgressPercentage, type: progressType },
      mainWindow.webContents
    );
  };

  const setBlockReplayProgressCheckingInterval = () => {
    setInterval(async () => {
      checkBlockReplayProgress();
    }, BLOCK_REPLAY_PROGRESS_CHECK_INTERVAL);
  };

  // Start default interval
  setBlockReplayProgressCheckingInterval();

  return checkBlockReplayProgress;
};
