// @flow
import React from 'react';

import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import { defineMessages, injectIntl, intlShape } from 'react-intl';

import { VOTING_CURRENT_FUND_END_DATE } from '../../../config/votingConfig';
import { formattedDateTime } from '../../../utils/formatters';
import type { Locale } from '../../../../../common/types/locales.types';

import customLinkThemeOverrides from './theme-overrides/customLink.scss';

import styles from './CurrentFund.scss';

const messages = defineMessages({
  name: {
    id: 'voting.currentFund.name',
    defaultMessage: '!!!Fund6',
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
    description: 'View resuls link label for Fund6',
  },
  viewResultsLinkURL: {
    id: 'voting.currentFund.viewResultsLinkURL',
    defaultMessage: '!!!https://TODO',
    description: 'View results from Fund6',
  },
});

type Props = {
  currentLocale: Locale,
  currentDateFormat: string,
  currentTimeFormat: string,
  onExternalLinkClick: Function,
  intl: intlShape.isRequired,
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
      <h1 className={styles.fundName}>{intl.formatMessage(messages.name)}</h1>

      <div className={styles.endDateBlock}>
        <span className={styles.endDateText}>
          {intl.formatMessage(messages.headingForEndDate)}
        </span>
        <span className={styles.endDate}>{currentFundEndDate}</span>
      </div>

      <Link
        label={intl.formatMessage(messages.viewResultsLinkLabel)}
        skin={LinkSkin}
        isUnderlined={false}
        themeOverrides={customLinkThemeOverrides}
        onClick={() => onExternalLinkClick(messages.viewResultsLinkURL)}
      />
    </section>
  );
}

export default injectIntl(CurrentFund);
