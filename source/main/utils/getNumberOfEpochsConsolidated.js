// @flow
import fs from 'fs';
import path from 'path';
import { nodeDbPath } from '../config';
import { getNumberOfEpochsConsolidatedChannel } from '../ipc/getNumberOfEpochsConsolidated.ipc';
import type { GetNumberOfEpochsConsolidatedChannelResponse } from '../../common/ipc/api';

console.log('nodeDbPath', nodeDbPath);

export const getNumberOfEpochsConsolidated = () => {
  getNumberOfEpochsConsolidatedChannel
    .onRequest((): Promise<GetNumberOfEpochsConsolidatedChannelResponse> => {
      const epochsPath = path.join(nodeDbPath, 'epochs');
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
