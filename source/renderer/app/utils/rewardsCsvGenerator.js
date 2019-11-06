// @flow
import { generateRewardsCsvChannel } from '../ipc/generateRewardsCsvChannel';
import type { CsvRecord } from '../../../common/types/rewards-csv-request.types';

type Params = {
  rewards: Array<CsvRecord>,
  filePath: string,
};

export const downloadRewardsCsv = async ({ rewards, filePath }: Params) => {
  await generateRewardsCsvChannel.send({
    rewards,
    filePath,
  });
};
