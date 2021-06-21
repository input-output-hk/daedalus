// @flow
import path from 'path';
import { intlShape, defineMessages } from 'react-intl';
import { generateFileNameWithTimestamp } from '../../../common/utils/files';
import { showSaveDialogChannel } from '../ipc/show-file-dialog-channels';
import { downloadCsv } from './csvGenerator';
import type { RewardsHistoryItem } from '../api/staking/types';

const messages = defineMessages({
  columnEpoch: {
    id: 'staking.rewardsHistory.csv.column.epoch',
    defaultMessage: '!!!Epoch',
    description: 'Transactions CSV column - Epoch',
  },
  columnStakePool: {
    id: 'staking.rewardsHistory.csv.column.stakePool',
    defaultMessage: '!!!Stake pool',
    description: 'Transactions CSV column - Pool',
  },
  columnAmount: {
    id: 'staking.rewardsHistory.csv.column.amount',
    defaultMessage: '!!!Amount',
    description: 'Transactions CSV column - Amount',
  },
  filenamePrefix: {
    id: 'staking.rewardsHistory.csv.filenamePrefix',
    defaultMessage: '!!!Rewards',
    description: 'Rewards history CSV "Rewards" filename',
  },
});

type Params = {
  desktopDirectoryPath: string,
  intl: intlShape,
  rewards: Array<RewardsHistoryItem>,
  walletName: string,
};

const rewardsCsvGenerator = async ({
  desktopDirectoryPath,
  intl,
  rewards,
  walletName,
}: Params): Promise<boolean> => {
  const prefix = `${intl.formatMessage(messages.filenamePrefix)}-${walletName}`;
  const fileName = generateFileNameWithTimestamp({
    prefix,
    extension: 'csv',
    isUTC: true,
  });
  const defaultPath = path.join(desktopDirectoryPath, fileName);
  const params = {
    defaultPath,
    filters: [
      {
        extensions: ['csv'],
      },
    ],
  };
  const { filePath } = await showSaveDialogChannel.send(params);

  // if cancel button is clicked or path is empty
  if (!filePath) return false;

  const columns = [
    intl.formatMessage(messages.columnEpoch),
    intl.formatMessage(messages.columnStakePool),
    intl.formatMessage(messages.columnAmount),
  ];

  const fileContent = [columns];
  rewards.forEach((reward: RewardsHistoryItem) => {
    const { epoch, pool, amount } = reward;
    const valueEpoch = epoch.toString();
    const valuePool = pool ? pool.ticker : '';
    if (!pool) {
      console.log('NO REWARD POOL: ', reward);
    }
    const valueAmount = amount.toFormat(6);
    const values = [valueEpoch, valuePool, valueAmount];
    fileContent.push(values);
  });

  await downloadCsv({ filePath, fileContent });
  return true;
};

export default rewardsCsvGenerator;
