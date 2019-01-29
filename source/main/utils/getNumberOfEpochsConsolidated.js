// @flow
import fs from 'fs';
import path from 'path';
import { appFolderPath } from '../config';
import { getNumberOfEpochsConsolidatedChannel } from '../ipc/getNumberOfEpochsConsolidated.ipc';
import type { GetNumberOfEpochsConsolidatedChannelResponse } from '../../common/ipc/api';
import { environment } from '../environment';

const { isLinux } = environment;

export const getNumberOfEpochsConsolidated = () => {
  getNumberOfEpochsConsolidatedChannel
    .onRequest((): Promise<GetNumberOfEpochsConsolidatedChannelResponse> => {
      const epochsPath = isLinux
        ? path.join(appFolderPath, 'DB', 'epochs')
        : path.join(appFolderPath, 'DB-1.0', 'epochs');
      let latestConsolidatedEpoch = 0;
      if (fs.existsSync(epochsPath)) {
        const epochfiles = fs
          .readdirSync(epochsPath)
          .filter(file => file.indexOf('.epoch') > -1)
          .map(file => parseInt(file.split('.').shift(), 10));
        if (epochfiles.length) latestConsolidatedEpoch = Math.max(...epochfiles);
      }
      return Promise.resolve(latestConsolidatedEpoch);
    });
};
