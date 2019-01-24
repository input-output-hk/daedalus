// @flow
import { BrowserWindow } from 'electron';
import fs from 'fs';
import path from 'path';
import { appFolderPath } from '../config';
import { getNumberOfEpochsConsolidatedChannel } from '../ipc/getNumberOfEpochsConsolidated.ipc';
import type { GetNumberOfEpochsConsolidatedChannelResponse } from '../../common/ipc/api';


export const getNumberOfEpochsConsolidated = (mainWindow: BrowserWindow) => {
  getNumberOfEpochsConsolidatedChannel
    .onReceive((): Promise<GetNumberOfEpochsConsolidatedChannelResponse> => {
      const epochsPath = path.join(appFolderPath, 'DB-1.0/epochs');
      let epochsConsolidatedLength = 123;
      if (fs.existsSync(epochsPath)) {

        const epochsConsolidated = fs
          .readdirSync(epochsPath)
          .filter(file => file.indexOf('.epoch') > -1);

        epochsConsolidatedLength = epochsConsolidated.length;
      }
      getNumberOfEpochsConsolidatedChannel.send(
        epochsConsolidatedLength,
        mainWindow.webContents
      );
      return Promise.resolve(epochsConsolidatedLength);
    });
};
