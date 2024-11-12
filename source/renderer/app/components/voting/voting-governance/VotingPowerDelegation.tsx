import React from 'react';
import { observer } from 'mobx-react';
import { injectIntl } from 'react-intl';
import BorderedBox from '../../widgets/BorderedBox';
import { messages } from '../voting-info/Headline.messages';
import styles from './VotingPowerDelegation.scss';
import type { Intl } from '../../../types/i18nTypes';

type Props = {
  intl: Intl;
};

function VotingPowerDelegation({ intl }: Props) {
  return (
    <div className={styles.component}>
      <BorderedBox>
        <h1 className={styles.heading}>
          {intl.formatMessage(messages.heading)}
        </h1>
      </BorderedBox>
    </div>
  );
}

export default observer(injectIntl(VotingPowerDelegation));
