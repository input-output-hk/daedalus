import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { FormattedHTMLMessage } from 'react-intl';
import BigNumber from 'bignumber.js';
import globalMessages from '../../i18n/global-messages';
import LoadingSpinner from '../widgets/LoadingSpinner';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './StakingUnavailable.scss' or ... Remove this comment to see the full error message
import styles from './StakingUnavailable.scss';

type Props = {
  syncPercentage: number;
};

@observer
class StakingUnavailable extends Component<Props> {
  render() {
    const { syncPercentage } = this.props;
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
  }
}

export default StakingUnavailable;
