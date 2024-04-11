import cx from 'classnames';
import React from 'react';
import BigNumber from 'bignumber.js';
import { intlShape } from 'react-intl';
import { BlockSyncType } from '../../../../../../common/types/cardano-node.types';
import { Intl } from '../../../../types/i18nTypes';
import styles from './SyncingProgress.scss';

import { logger } from '../../../../utils/logging';

type Props = Record<BlockSyncType, number>;

interface Context {
  intl: Intl;
}

const blockSyncTypesOrdered: Array<BlockSyncType> = [
  BlockSyncType.validatingChunk,
  BlockSyncType.replayedBlock,
  BlockSyncType.pushingLedger,
];

const makeLeftColumnIconStyles = (loading: boolean) =>
  cx(styles.icon, styles.faded, {
    [styles.iconRotating]: loading,
  });
const makeMainMessageStyles = (loaded: boolean) =>
  cx({ [styles.faded]: loaded });
const makePercentageCellStyles = (loaded: boolean) =>
  cx(styles.cell, styles.cellTextRight, {
    [styles.faded]: loaded,
  });

const getSafePercentage = (value: number): string => {
  try {
    return new BigNumber(value).toFixed(2).toString();
  } catch (error) {
    logger.error('SyncingProgress::Percentage::Error parsing sync percentage', {
      error,
    });
    return '-';
  }
};

function SyncingProgress(props: Props, { intl }: Context) {
  return <div className={styles.root} />;
}

SyncingProgress.contextTypes = {
  intl: intlShape.isRequired,
};

export default SyncingProgress;
