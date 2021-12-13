import React from 'react';
import { observer } from 'mobx-react';
import { FormattedHTMLMessage } from 'react-intl';
import globalMessages from '../../i18n/global-messages';
import LoadingSpinner from '../widgets/LoadingSpinner';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './VotingUnavailable.scss' or i... Remove this comment to see the full error message
import styles from './VotingUnavailable.scss';
import { formattedNumber } from '../../utils/formatters';

type Props = {
  syncPercentage: number;
};

const VotingUnavailable = ({ syncPercentage }: Props) => {
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
};

export default observer(VotingUnavailable);
