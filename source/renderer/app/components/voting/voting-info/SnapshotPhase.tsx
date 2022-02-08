import React from 'react';
import { observer } from 'mobx-react';
import { injectIntl } from 'react-intl';
import {
  formattedDateTime,
  mapToLongDateTimeFormat,
} from '../../../utils/formatters';
import type { Locale } from '../../../../../common/types/locales.types';
import type { Intl } from '../../../types/i18nTypes';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './CurrentPhase.scss' or its co... Remove this comment to see the full error message
import styles from './CurrentPhase.scss';
import { messages } from './SnapshotPhase.messages';
import { messages as votingMessages } from './VotingInfo.messages';
import type { CatalystFund } from '../../../api/voting/types';

type Props = {
  currentLocale: Locale;
  currentDateFormat: string;
  currentTimeFormat: string;
  fundInfo: CatalystFund;
  intl: Intl;
};

function SnapshotPhase({
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
  const snapshotDate = formattedDateTime(
    fundInfo.current.registrationSnapshotTime,
    {
      currentLocale,
      currentDateFormat: mappedFormats.currentDateFormat,
      currentTimeFormat: mappedFormats.currentTimeFormat,
    }
  );
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
          {intl.formatMessage(messages.snapshotDateLabel)}
        </span>
        <span className={styles.value}>{snapshotDate}</span>
      </div>
      <div className={styles.block}>
        <span className={styles.label}>
          {intl.formatMessage(messages.votingDateLabel)}
        </span>
        <span className={styles.value}>
          {startDate} – {endDate}
        </span>
      </div>
    </section>
  );
}

export default injectIntl(observer(SnapshotPhase));
