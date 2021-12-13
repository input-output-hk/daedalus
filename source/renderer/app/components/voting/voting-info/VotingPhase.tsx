import React from 'react';
import { observer } from 'mobx-react';
import { injectIntl } from 'react-intl';
import {
  CURRENT_VOTING_FUND_NUMBER,
  VOTING_CAST_START_DATE,
  VOTING_CAST_END_DATE,
} from '../../../config/votingConfig';
import {
  formattedDateTime,
  mapToLongDateTimeFormat,
} from '../../../utils/formatters';
import type { Locale } from '../../../../../common/types/locales.types';
import type { Intl } from '../../../types/i18nTypes';
import { messages } from './VotingPhase.messages';
import { messages as votingMessages } from './VotingInfo.messages';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './CurrentPhase.scss' or its co... Remove this comment to see the full error message
import styles from './CurrentPhase.scss';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './VotingInfo.scss' or its corr... Remove this comment to see the full error message
import votingStyles from './VotingInfo.scss';

type Props = {
  currentLocale: Locale;
  currentDateFormat: string;
  currentTimeFormat: string;
  intl: Intl;
};

function VotingPhase({
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
  const startDate = formattedDateTime(VOTING_CAST_START_DATE, {
    currentLocale,
    currentDateFormat: mappedFormats.currentDateFormat,
  });
  const endDate = formattedDateTime(VOTING_CAST_END_DATE, {
    currentLocale,
    currentDateFormat: mappedFormats.currentDateFormat,
  });
  return (
    <section className={styles.root}>
      <h1 className={styles.fundName}>
        {intl.formatMessage(votingMessages.fundName, {
          votingFundNumber: CURRENT_VOTING_FUND_NUMBER,
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
