import React from 'react';
import { observer } from 'mobx-react';
import { injectIntl } from 'react-intl';
import {
  formattedDateTime,
  mapToLongDateTimeFormat,
} from '../../../utils/formatters';
import { ExternalLinkButton } from '../../widgets/ExternalLinkButton';
import { messages } from './ResultsPhase.messages';
import { messages as votingMessages } from './VotingInfo.messages';
import styles from './CurrentPhase.scss';
import type { PhaseIntlProps as Props } from './types';

function ResultsPhase({
  currentLocale,
  currentDateFormat,
  currentTimeFormat,
  fundInfo,
  onExternalLinkClick,
  intl,
}: Props) {
  const mappedFormats = mapToLongDateTimeFormat({
    currentLocale,
    currentDateFormat,
    currentTimeFormat,
  });
  const endDate = formattedDateTime(fundInfo.current.endTime, {
    currentLocale,
    currentDateFormat: mappedFormats.currentDateFormat,
    currentTimeFormat: mappedFormats.currentTimeFormat,
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
