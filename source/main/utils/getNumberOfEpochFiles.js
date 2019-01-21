import { BrowserWindow } from 'electron';
import fs from 'fs';
import path from 'path';
import { appFolderPath } from '../config';
import { getNumberOfEpochFilesChannel } from '../ipc/get-number-of-epoch-files.ipc';


export const getNumberOfEpochFiles = (mainWindow: BrowserWindow) => {
  getNumberOfEpochFilesChannel.onReceive(() => {
    const epochsPath = path.join(appFolderPath, 'DB-1.0/epochs');
    if (fs.existsSync(epochsPath)) {

      const epochFiles = fs
        .readdirSync(epochsPath)
        .filter(file => file.indexOf('.epoch') > -1);

      getNumberOfEpochFilesChannel.send(epochFiles.length, mainWindow.webContents);
    }
  });
};
