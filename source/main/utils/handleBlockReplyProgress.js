// @flow
import { BrowserWindow } from 'electron';
import fs from 'fs';
import readline from 'readline';
import { getBlockReplyProgressChannel } from '../ipc/get-block-reply-progress';
import { logger } from './logging';
import { BLOCK_REPLY_PROGRESS_CHECK_INTERVAL } from '../config';

export const handleBlockReplyProgress = (
  mainWindow: BrowserWindow,
  logsDirectoryPath: string
) => {
  let blockReplyProgressCheckInterval;

  const handleCheckBlockReplyProgress = async () => {
    const filePath = logsDirectoryPath + '/node.log';
    if(!fs.existsSync(filePath)) return;
    const fileStream = fs.createReadStream(filePath);

    let finalProgressPercentage = 0;
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
      finalProgressPercentage = parseFloat(percentage)

      getBlockReplyProgressChannel.send(finalProgressPercentage, mainWindow.webContents);

    logger.info('>>> finalProgressPercentage: ', { finalProgressPercentage: finalProgressPercentage })
    return finalProgressPercentage;
  };

  const setBlockReplyProgressIntervalChecking = () => {
    blockReplyProgressCheckInterval = setInterval(async () => {
      handleCheckBlockReplyProgress();
    }, BLOCK_REPLY_PROGRESS_CHECK_INTERVAL);
  };

  // Start default interval
  setBlockReplyProgressIntervalChecking();

  return handleCheckBlockReplyProgress;
};