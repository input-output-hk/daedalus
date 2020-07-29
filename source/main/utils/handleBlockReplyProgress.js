// @flow
import { BrowserWindow } from 'electron';
import fs from 'fs';
import readline from 'readline';
import path from 'path';
import { getBlockReplyProgressChannel } from '../ipc/get-block-reply-progress';
import { logger } from './logging';
import { BLOCK_REPLY_PROGRESS_CHECK_INTERVAL } from '../config';

export const handleBlockReplyProgress = (
  mainWindow: BrowserWindow,
  logsDirectoryPath: string
) => {
  const handleCheckBlockReplyProgress = async () => {
    const filename = 'node.log';
    const filePath = path.join(logsDirectoryPath, filename);
    if(!fs.existsSync(filePath)) return;

    const fileStream = fs.createReadStream(filePath);
    const rl = readline.createInterface({ input: fileStream });
    const progress = [];
    for await (const line of rl) {
      if (line.includes("block replay")) {
        progress.push(line);
      }
    }
    if (!progress.length) return;

    const filnalProgress = progress.slice(-1).pop();
    const percentage = filnalProgress.split('block replay progress (%) =').pop();
    const finalProgressPercentage = parseFloat(percentage)

    // Send result to renderer process (NetworkStatusStore)
    getBlockReplyProgressChannel.send(finalProgressPercentage, mainWindow.webContents);
  };

  const setBlockReplyProgressIntervalChecking = () => {
    setInterval(async () => {
      handleCheckBlockReplyProgress();
    }, BLOCK_REPLY_PROGRESS_CHECK_INTERVAL);
  };

  // Start default interval
  setBlockReplyProgressIntervalChecking();

  return handleCheckBlockReplyProgress;
};