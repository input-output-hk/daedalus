// @flow
import React, { useState } from 'react';
import { injectIntl } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import {
  VOTING_NEW_SNAPSHOT_DATE,
  NEXT_VOTING_FUND_NUMBER,
} from '../../../config/votingConfig';
import {
  formattedDateTime,
  mapToLongDateTimeFormat,
} from '../../../utils/formatters';
import type { Locale } from '../../../../../common/types/locales.types';
import type { Intl } from '../../../types/i18nTypes';
import styles from './RegisterToVote.scss';
import { messages } from './RegisterToVote.messages';

type Props = {
  currentLocale: Locale,
  currentDateFormat: string,
  currentTimeFormat: string,
  intl: Intl,
  onRegisterToVoteClick: Function,
};

function RegisterToVote({
  currentLocale,
  currentDateFormat,
  currentTimeFormat,
  intl,
  onRegisterToVoteClick,
}: Props) {
  const [step1, setStep1] = useState(false);
  const [step2, setStep2] = useState(false);
  const canRegister = step1 && step2;
  const castEndDate = formattedDateTime(VOTING_NEW_SNAPSHOT_DATE, {
    currentLocale,
    ...mapToLongDateTimeFormat({
      currentLocale,
      currentDateFormat,
      currentTimeFormat,
    }),
  });

  return (
    <div className={styles.root}>
      <span className={styles.title}>
        {intl.formatMessage(messages.name, {
          nextVotingFundNumber: NEXT_VOTING_FUND_NUMBER,
        })}
      </span>
      <span className={styles.dateLabel}>
        {intl.formatMessage(messages.dateLabel)}
      </span>
      <span className={styles.date}>{castEndDate}</span>
      <hr />
      <span className={styles.stepsTitle}>
        {intl.formatMessage(messages.stepsTitle)}
      </span>
      <div className={styles.step}>
        <Checkbox
          className={styles.checkbox}
          checked={step1}
          onChange={setStep1}
          label={intl.formatMessage(messages.step1CheckBoxLabel)}
        />
      </div>
      <div className={styles.step}>
        <Checkbox
          className={styles.checkbox}
          checked={step2}
          label={intl.formatMessage(messages.step2CheckBoxLabel)}
          onChange={setStep2}
        />
      </div>
      <Button
        className={styles.button}
        onClick={onRegisterToVoteClick}
        label={intl.formatMessage(messages.buttonLabel)}
        disabled={!canRegister}
      />
    </div>
  );
}

export default injectIntl(RegisterToVote);
