import React, { useState } from 'react';
import { injectIntl } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import type { Intl } from '../../../types/i18nTypes';
import { messages } from './RegisterToVote.messages';
import { messages as votingMessages } from './VotingInfo.messages';
import styles from './RegisterToVote.scss';

type Props = {
  intl: Intl;
  onRegisterToVoteClick: (...args: Array<any>) => any;
};

function RegisterToVote({ intl, onRegisterToVoteClick }: Props) {
  const [step1, setStep1] = useState(false);
  const [step2, setStep2] = useState(false);
  const canRegister = step1 && step2;

  return (
    <div className={styles.root}>
      <div className={styles.title}>
        {intl.formatMessage(votingMessages.registerToVoteHeader)}
      </div>
      <div className={styles.votingInstructions}>
        {intl.formatMessage(messages.votingInstructions)}
      </div>
      <div className={styles.stepsTitle}>
        {intl.formatMessage(messages.stepsTitle)}
      </div>
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
    </div>
  );
}

export default injectIntl(RegisterToVote);
