// @flow
import React from 'react';
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
import styles from './CurrentPhase.scss';
import { messages } from './FundResults.messages';

type Props = {
  currentLocale: Locale,
  currentDateFormat: string,
  currentTimeFormat: string,
  onExternalLinkClick: Function,
  intl: Intl,
};

function FundResults({
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
        {intl.formatMessage(messages.currentFundName, {
          currentVotingFundNumber: CURRENT_VOTING_FUND_NUMBER,
        })}
      </h1>

      <div className={styles.block}>
        <span className={styles.label}>
          {intl.formatMessage(messages.date)}
        </span>
        <span className={styles.value}>{endDate}</span>
      </div>

      <ExternalLinkButton
        label={intl.formatMessage(messages.viewResultsLinkLabel)}
        onClick={() =>
          onExternalLinkClick(intl.formatMessage(messages.viewResultsLinkURL))
        }
      />
    </section>
  );
}

export default injectIntl(FundResults);
