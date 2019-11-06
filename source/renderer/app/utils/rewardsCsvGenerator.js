// @flow
import { generateRewardsCsvChannel } from '../ipc/generateRewardsCsvChannel';

type Params = {
  rewards: Array<Array<string>>,
  filePath: string,
};

export const downloadRewardsCsv = async ({ rewards, filePath }: Params) => {
  await generateRewardsCsvChannel.send({
    rewards,
    filePath,
  });
};
