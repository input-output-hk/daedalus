// @flow
import React from 'react';
import { observer } from 'mobx-react';
import { FormattedHTMLMessage } from 'react-intl';
import BigNumber from 'bignumber.js';
import globalMessages from '../../i18n/global-messages';
import LoadingSpinner from '../widgets/LoadingSpinner';
import styles from './VotingUnavailable.scss';

type Props = {
  syncPercentage: number,
};

const VotingUnavailable = ({ syncPercentage }: Props) => {
  return (
    <div className={styles.component}>
      <LoadingSpinner big />
      <div className={styles.description}>
        <FormattedHTMLMessage
          {...globalMessages.featureUnavailableWhileSyncing}
          values={{
            syncPercentage: new BigNumber(syncPercentage).toFormat(2),
          }}
        />
      </div>
    </div>
  );
};

export default observer(VotingUnavailable);
