import React from 'react';
import { observer } from 'mobx-react';
import { injectIntl } from 'react-intl';
import {
  formattedDateTime,
  mapToLongDateTimeFormat,
} from '../../../utils/formatters';
import { messages } from './VotingPhase.messages';
import { messages as votingMessages } from './VotingInfo.messages';
import styles from './CurrentPhase.scss';
import votingStyles from './VotingInfo.scss';
import type { PhaseIntlProps as Props } from './types';

function VotingPhase({
  currentLocale,
  currentDateFormat,
  currentTimeFormat,
  fundInfo,
  intl,
}: Props) {
  const mappedFormats = mapToLongDateTimeFormat({
    currentLocale,
    currentDateFormat,
    currentTimeFormat,
  });
  const startDate = formattedDateTime(fundInfo.current.startTime, {
    currentLocale,
    currentDateFormat: mappedFormats.currentDateFormat,
  });
  const endDate = formattedDateTime(fundInfo.current.endTime, {
    currentLocale,
    currentDateFormat: mappedFormats.currentDateFormat,
  });
  return (
    <section className={styles.root}>
      <h1 className={styles.fundName}>
        {intl.formatMessage(votingMessages.fundName, {
          votingFundNumber: fundInfo.current.number,
        })}
      </h1>
      <div className={styles.block}>
        <span className={styles.label}>
          {intl.formatMessage(messages.dateLabel)}
        </span>
        <span className={styles.value}>
          {startDate} – {endDate}
        </span>
      </div>
      <hr className={votingStyles.separator} />
      <div className={styles.block}>
        <span className={styles.value}>
          {intl.formatMessage(messages.instruction)}
        </span>
      </div>
    </section>
  );
}

export default injectIntl(observer(VotingPhase));
