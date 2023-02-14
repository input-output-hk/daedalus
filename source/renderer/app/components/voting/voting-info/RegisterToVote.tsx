import React from 'react';
import moment from 'moment';
import { injectIntl } from 'react-intl';
import type { Intl } from '../../../types/i18nTypes';
import NextFund from './NextFund';

import type { Locale } from '../../../../../common/types/locales.types';
import { messages } from './RegisterToVote.messages';
import { messages as votingMessages } from './VotingInfo.messages';
import styles from './RegisterToVote.scss';
import type { CatalystFund } from '../../../api/voting/types';
import { logger } from '../../../utils/logging';

type Props = {
  currentLocale: Locale;
  currentDateFormat: string;
  currentTimeFormat: string;
  fundInfo: CatalystFund;
  intl: Intl;
  onRegisterToVoteClick: (...args: Array<any>) => any;
};

const isNextFundDefined = (registrationSnapshotTime: Date): boolean => {
  try {
    return moment().diff(registrationSnapshotTime) < 0;
  } catch (error) {
    logger.error('Voting::NextFund::Invalid date', {
      error,
    });
  }
  return false;
};

// const NextFund = React.lazy(() => import('./NextFund'));

export function RegisterToVote({
  currentLocale,
  currentDateFormat,
  currentTimeFormat,
  fundInfo,
  intl,
  onRegisterToVoteClick,
}: Props) {
  // const isNextFundDefined = false;
  return (
    <div className={styles.root}>
      <span className={styles.title}>
        {intl.formatMessage(votingMessages.fundName, {
          votingFundNumber: fundInfo.next.number,
        })}
      </span>
      {isNextFundDefined(fundInfo?.next?.registrationSnapshotTime) ? (
        <NextFund
          currentLocale={currentLocale}
          currentDateFormat={currentDateFormat}
          currentTimeFormat={currentTimeFormat}
          fundInfo={fundInfo}
          onRegisterToVoteClick={onRegisterToVoteClick}
        />
      ) : (
        <React.Fragment>
          <span className={styles.dateLabel}>
            {intl.formatMessage(messages.dateLabel)}
          </span>
          <span className={styles.date}>
            {intl.formatMessage(messages.toBeDefined)}
          </span>
        </React.Fragment>
      )}
    </div>
  );
}
export default injectIntl(RegisterToVote);
