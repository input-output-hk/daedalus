// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, FormattedHTMLMessage } from 'react-intl';
import styles from './VotingUnavailable.scss';

const messages = defineMessages({
  description: {
    id: 'voting.unavailable.description',
    defaultMessage:
      '!!!Before registering for voting, Daedalus first needs to synchronize with the blockchain. The synchronization process is now underway and it is currently {syncPercentage}% complete. As soon as this process is fully complete, you’ll be able to register with Fund3. Please wait for this process to complete before returning here.',
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
        <div className={styles.description}>
          <FormattedHTMLMessage
            {...messages.description}
            values={{
              syncPercentage: parseFloat(syncPercentage).toFixed(2),
            }}
          />
        </div>
      </div>
    );
  }
}
