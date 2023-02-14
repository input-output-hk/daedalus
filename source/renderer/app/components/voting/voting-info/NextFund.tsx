import React, { useState } from 'react';
import { injectIntl } from 'react-intl';

import { Button } from 'react-polymorph/lib/components/Button';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import {
  formattedDateTime,
  mapToLongDateTimeFormat,
} from '../../../utils/formatters';
import type { Locale } from '../../../../../common/types/locales.types';
import { messages } from './RegisterToVote.messages';
import styles from './RegisterToVote.scss';
import votingStyles from './VotingInfo.scss';
import type { CatalystFund } from '../../../api/voting/types';
import type { Intl } from '../../../types/i18nTypes';

type Props = {
  currentLocale: Locale;
  currentDateFormat: string;
  currentTimeFormat: string;
  fundInfo: CatalystFund;
  intl: Intl;
  onRegisterToVoteClick: (...args: Array<any>) => any;
};

function NextFund({
  currentLocale,
  currentDateFormat,
  currentTimeFormat,
  fundInfo,
  intl,
  onRegisterToVoteClick,
}: Props) {
  const [step1, setStep1] = useState(false);
  const [step2, setStep2] = useState(false);
  const canRegister = step1 && step2;
  const snapshotDate = formattedDateTime(
    fundInfo.next.registrationSnapshotTime,
    {
      currentLocale,
      ...mapToLongDateTimeFormat({
        currentLocale,
        currentDateFormat,
        currentTimeFormat,
      }),
    }
  );
  return (
    <React.Fragment>
      <span className={styles.dateLabel}>
        {intl.formatMessage(messages.dateLabel)}
      </span>
      <span className={styles.date}>{snapshotDate}</span>
      <hr className={votingStyles.separator} />
      <span className={styles.stepsTitle}>
        {intl.formatMessage(messages.stepsTitle)}
      </span>
      <div className={styles.step}>
        <Checkbox
          checked={step1}
          onChange={setStep1}
          label={intl.formatMessage(messages.step1CheckBoxLabel)}
        />
      </div>
      <div className={styles.step}>
        <Checkbox
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
    </React.Fragment>
  );
}

export default injectIntl(NextFund);
