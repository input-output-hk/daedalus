import { BrowserWindow } from 'electron';
import fs from 'fs';
import path from 'path';
import { appFolderPath } from '../config';
import { getNumberOfEpochsConsolidatedChannel } from '../ipc/get-number-of-epochs-consolidated.ipc';


export const getNumberOfEpochsConsolidated = (mainWindow: BrowserWindow) => {
  getNumberOfEpochsConsolidatedChannel.onReceive(() => {
    const epochsPath = path.join(appFolderPath, 'DB-1.0/epochs');
    if (fs.existsSync(epochsPath)) {

      const epochsConsolidated = fs
        .readdirSync(epochsPath)
        .filter(file => file.indexOf('.epoch') > -1);

      getNumberOfEpochsConsolidatedChannel.send(epochsConsolidated.length, mainWindow.webContents);
    }
  });
};
