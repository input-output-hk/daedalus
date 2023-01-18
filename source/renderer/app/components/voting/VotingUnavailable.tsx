import React from 'react';
import { observer } from 'mobx-react';
import { FormattedHTMLMessage } from 'react-intl';
import globalMessages from '../../i18n/global-messages';
import LoadingSpinner from '../widgets/LoadingSpinner';
import styles from './VotingUnavailable.scss';
import { formattedNumber } from '../../utils/formatters';

type Props = {
  syncPercentage: number;
};

function VotingUnavailable({ syncPercentage }: Props) {
  return (
    <div className={styles.component}>
      <LoadingSpinner big />
      <div className={styles.description}>
        <FormattedHTMLMessage
          {...globalMessages.featureUnavailableWhileSyncing}
          values={{
            syncPercentage: formattedNumber(syncPercentage, 2),
          }}
        />
      </div>
    </div>
  );
}

export default observer(VotingUnavailable);
