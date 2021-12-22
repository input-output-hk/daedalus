import React from 'react';
import { observer } from 'mobx-react';
import { injectIntl } from 'react-intl';
import {
  CURRENT_VOTING_FUND_NUMBER,
  VOTING_RESULTS_DATE,
  VOTING_CAST_END_DATE,
} from '../../../config/votingConfig';
import {
  formattedDateTime,
  mapToLongDateTimeFormat,
} from '../../../utils/formatters';
import type { Locale } from '../../../../../common/types/locales.types';
import type { Intl } from '../../../types/i18nTypes';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './CurrentPhase.scss' or its co... Remove this comment to see the full error message
import styles from './CurrentPhase.scss';
import { messages } from './TallyingPhase.messages';
import { messages as votingMessages } from './VotingInfo.messages';

type Props = {
  currentLocale: Locale;
  currentDateFormat: string;
  currentTimeFormat: string;
  intl: Intl;
};

function TallyingPhase({
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
  const endDated = formattedDateTime(VOTING_CAST_END_DATE, {
    currentLocale,
    currentDateFormat: mappedFormats.currentDateFormat,
  });
  const resultsDate = formattedDateTime(VOTING_RESULTS_DATE, {
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
          {intl.formatMessage(messages.endDateLabel)}
        </span>
        <span className={styles.value}>{endDated}</span>
      </div>
      <div className={styles.block}>
        <span className={styles.label}>
          {intl.formatMessage(messages.resultsLabel)}
        </span>
        <span className={styles.value}>{resultsDate}</span>
      </div>
    </section>
  );
}

export default injectIntl(observer(TallyingPhase));
