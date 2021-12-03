// @flow
import React from 'react';
import { observer } from 'mobx-react';
import { injectIntl } from 'react-intl';
import {
  CURRENT_VOTING_FUND_NUMBER,
  VOTING_SNAPSHOT_DATE,
  VOTING_CAST_START_DATE,
  VOTING_CAST_END_DATE,
} from '../../../config/votingConfig';
import {
  formattedDateTime,
  mapToLongDateTimeFormat,
} from '../../../utils/formatters';
import type { Locale } from '../../../../../common/types/locales.types';
import type { Intl } from '../../../types/i18nTypes';
import styles from './CurrentPhase.scss';
import { messages } from './SnapshotPhase.messages';
import { messages as currentPhase } from './CurrentPhase.messages';

type Props = {
  currentLocale: Locale,
  currentDateFormat: string,
  currentTimeFormat: string,
  intl: Intl,
};

function SnapshotPhase({
  currentLocale,
  currentDateFormat,
  currentTimeFormat,
  intl,
}: Props) {
  const mappedFormats = mapToLongDateTimeFormat({
    currentLocale,
    currentDateFormat,
    currentTimeFormat,
  });

  const upcomingFundSnapshotDate = formattedDateTime(VOTING_SNAPSHOT_DATE, {
    currentLocale,
    currentDateFormat: mappedFormats.currentDateFormat,
    currentTimeFormat: mappedFormats.currentTimeFormat,
  });

  const upcomingFundStartDate = formattedDateTime(VOTING_CAST_START_DATE, {
    currentLocale,
    currentDateFormat: mappedFormats.currentDateFormat,
  });

  const upcomingFundEndDate = formattedDateTime(VOTING_CAST_END_DATE, {
    currentLocale,
    currentDateFormat: mappedFormats.currentDateFormat,
  });

  return (
    <section className={styles.root}>
      <h1 className={styles.fundName}>
        {intl.formatMessage(currentPhase.currentFundName, {
          currentVotingFundNumber: CURRENT_VOTING_FUND_NUMBER,
        })}
      </h1>
      <div className={styles.block}>
        <span className={styles.label}>
          {intl.formatMessage(messages.snapshotDateLabel)}
        </span>
        <span className={styles.value}>{upcomingFundSnapshotDate}</span>
      </div>
      <div className={styles.block}>
        <span className={styles.label}>
          {intl.formatMessage(messages.votingDateLabel)}
        </span>
        <span className={styles.value}>
          {upcomingFundStartDate} – {upcomingFundEndDate}
        </span>
      </div>
    </section>
  );
}

export default injectIntl(observer(SnapshotPhase));
