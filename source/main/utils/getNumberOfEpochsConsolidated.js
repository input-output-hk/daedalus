// @flow
import fs from 'fs';
import path from 'path';
import { appFolderPath } from '../config';
import { getNumberOfEpochsConsolidatedChannel } from '../ipc/getNumberOfEpochsConsolidated.ipc';
import type { GetNumberOfEpochsConsolidatedChannelResponse } from '../../common/ipc/api';

export const getNumberOfEpochsConsolidated = () => {
  getNumberOfEpochsConsolidatedChannel
    .onRequest((): Promise<GetNumberOfEpochsConsolidatedChannelResponse> => {
      const epochsPath = path.join(appFolderPath, 'DB-1.0/epochs');
      let epochsConsolidatedLength = 123;
      if (fs.existsSync(epochsPath)) {

        const epochsConsolidated = fs
          .readdirSync(epochsPath)
          .filter(file => file.indexOf('.epoch') > -1);

        epochsConsolidatedLength = epochsConsolidated.length;
      }
      return Promise.resolve(epochsConsolidatedLength);
    });
};
