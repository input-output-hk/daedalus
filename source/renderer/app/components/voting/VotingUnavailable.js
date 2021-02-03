// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, FormattedHTMLMessage } from 'react-intl';
import BigNumber from 'bignumber.js';
import LoadingSpinner from '../widgets/LoadingSpinner';
import styles from './VotingUnavailable.scss';

const messages = defineMessages({
  description: {
    id: 'voting.unavailable.description',
    defaultMessage:
      '!!!Daedalus is synchronizing with the Cardano blockchain, and the process is currently {syncPercentage}% complete. This feature will become available once Daedalus is fully synchronized.',
    description: 'Description for voting registration unavailable screen',
  },
});

type Props = {
  syncPercentage: number,
};

@observer
export default class VotingUnavailable extends Component<Props> {
  render() {
    const { syncPercentage } = this.props;

    return (
      <div className={styles.component}>
        <LoadingSpinner big />
        <div className={styles.description}>
          <FormattedHTMLMessage
            {...messages.description}
            values={{
              syncPercentage: new BigNumber(syncPercentage).toFormat(2),
            }}
          />
        </div>
      </div>
    );
  }
}
