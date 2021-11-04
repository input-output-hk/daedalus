// @flow
import React from 'react';

import { defineMessages, injectIntl } from 'react-intl';

import {
  VOTING_CURRENT_FUND_END_DATE,
  CURRENT_VOTING_FUND_NUMBER,
} from '../../../config/votingConfig';
import { formattedDateTime } from '../../../utils/formatters';
import type { Locale } from '../../../../../common/types/locales.types';
import { ExternalLinkButton } from '../../widgets/ExternalLinkButton';
import type { Intl } from '../../../types/i18nTypes';

import styles from './CurrentFund.scss';

const messages = defineMessages({
  name: {
    id: 'voting.currentFund.name',
    defaultMessage: '!!!Fund{currentVotingFundNumber}',
    description: 'Current fund name',
  },
  headingForEndDate: {
    id: 'voting.currentFund.headingForEndDate',
    defaultMessage: '!!!End date:',
    description: 'Headline for end date',
  },
  viewResultsLinkLabel: {
    id: 'voting.currentFund.viewResultsLinkLabel',
    defaultMessage: '!!!View results',
    description: 'View resuls link label for Fund{currentVotingFundNumber}',
  },
  viewResultsLinkURL: {
    id: 'voting.currentFund.viewResultsLinkURL',
    defaultMessage: '!!!https://TODO',
    description: 'View results from Fund{currentVotingFundNumber}',
  },
});

type Props = {
  currentLocale: Locale,
  currentDateFormat: string,
  currentTimeFormat: string,
  onExternalLinkClick: Function,
  intl: Intl,
};

function CurrentFund({
  currentLocale,
  currentDateFormat,
  currentTimeFormat,
  onExternalLinkClick,
  intl,
}: Props) {
  const currentFundEndDate = formattedDateTime(VOTING_CURRENT_FUND_END_DATE, {
    currentLocale,
    currentDateFormat,
    currentTimeFormat,
  });

  return (
    <section className={styles.component}>
      <h1 className={styles.fundName}>
        {intl.formatMessage(messages.name, {
          currentFundNumber: CURRENT_VOTING_FUND_NUMBER,
        })}
      </h1>

      <div className={styles.endDateBlock}>
        <span className={styles.endDateText}>
          {intl.formatMessage(messages.headingForEndDate)}
        </span>
        <span className={styles.endDate}>{currentFundEndDate}</span>
      </div>

      <ExternalLinkButton
        label={intl.formatMessage(messages.viewResultsLinkLabel)}
        onClick={() => onExternalLinkClick(messages.viewResultsLinkURL)}
      />
    </section>
  );
}

export default injectIntl(CurrentFund);
