import React from 'react';
import { observer } from 'mobx-react';
import { injectIntl } from 'react-intl';
import {
  VOTING_CAST_END_DATE,
  CURRENT_VOTING_FUND_NUMBER,
} from '../../../config/votingConfig';
import {
  formattedDateTime,
  mapToLongDateTimeFormat,
} from '../../../utils/formatters';
import type { Locale } from '../../../../../common/types/locales.types';
import { ExternalLinkButton } from '../../widgets/ExternalLinkButton';
import type { Intl } from '../../../types/i18nTypes';
import { messages } from './ResultsPhase.messages';
import { messages as votingMessages } from './VotingInfo.messages';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './CurrentPhase.scss' or its co... Remove this comment to see the full error message
import styles from './CurrentPhase.scss';

type Props = {
  currentLocale: Locale;
  currentDateFormat: string;
  currentTimeFormat: string;
  onExternalLinkClick: (...args: Array<any>) => any;
  intl: Intl;
};

function ResultsPhase({
  currentLocale,
  currentDateFormat,
  currentTimeFormat,
  onExternalLinkClick,
  intl,
}: Props) {
  const mappedFormats = mapToLongDateTimeFormat({
    currentLocale,
    currentDateFormat,
    currentTimeFormat,
  });
  const endDate = formattedDateTime(VOTING_CAST_END_DATE, {
    currentLocale,
    currentDateFormat: mappedFormats.currentDateFormat,
    currentTimeFormat: mappedFormats.currentTimeFormat,
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
        <span className={styles.value}>{endDate}</span>
      </div>

      <div className={styles.resultsButton}>
        <ExternalLinkButton
          label={intl.formatMessage(messages.viewResultsLinkLabel)}
          onClick={() =>
            onExternalLinkClick(intl.formatMessage(messages.viewResultsLinkURL))
          }
        />
      </div>
    </section>
  );
}

export default injectIntl(observer(ResultsPhase));
