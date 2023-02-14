import cx from 'classnames';
import React from 'react';
import BigNumber from 'bignumber.js';
import { intlShape } from 'react-intl';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import SVGInline from 'react-svg-inline';
import spinnerIcon from '../../../../assets/images/spinner-universal.inline.svg';
import checkMarkIcon from '../../../../assets/images/check-mark-universal.inline.svg';
import questionMarkIcon from '../../../../assets/images/question-mark-universal.inline.svg';
import { BlockSyncType } from '../../../../../../common/types/cardano-node.types';
import { Intl } from '../../../../types/i18nTypes';
import styles from './SyncingProgress.scss';
import {
  getProgressDescriptionByBlockSyncType,
  getProgressNameByBlockSyncType,
} from './utils';
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

const iconsColumnStyles = cx(styles.column, styles.columnIcons);
const messagesColumnStyles = cx(styles.column, styles.columnMessages);
const questionMarkIconStyles = cx(styles.icon, styles.iconDescription);
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
  return (
    <div className={styles.root}>
      <div className={iconsColumnStyles}>
        {blockSyncTypesOrdered.map((type) => (
          <div key={type} className={styles.cell}>
            <SVGInline
              svg={props[type] < 100 ? spinnerIcon : checkMarkIcon}
              className={makeLeftColumnIconStyles(props[type] < 100)}
            />
          </div>
        ))}
      </div>
      <div className={messagesColumnStyles}>
        {blockSyncTypesOrdered.map((type) => (
          <div key={type} className={styles.cell}>
            <span className={makeMainMessageStyles(props[type] === 100)}>
              {intl.formatMessage(getProgressNameByBlockSyncType(type))}
            </span>
            <span>
              <PopOver
                content={intl.formatMessage(
                  getProgressDescriptionByBlockSyncType(type)
                )}
              >
                <SVGInline
                  svg={questionMarkIcon}
                  className={questionMarkIconStyles}
                />
              </PopOver>
            </span>
          </div>
        ))}
      </div>
      <div className={styles.column}>
        {blockSyncTypesOrdered.map((type) => (
          <div
            key={type}
            className={makePercentageCellStyles(props[type] === 100)}
          >
            {getSafePercentage(props[type])}%
          </div>
        ))}
      </div>
    </div>
  );
}

SyncingProgress.contextTypes = {
  intl: intlShape.isRequired,
};

export default SyncingProgress;
