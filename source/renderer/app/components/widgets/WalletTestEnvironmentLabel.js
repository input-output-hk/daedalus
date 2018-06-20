import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import styles from './WalletTestEnvironmentLabel.scss';

const messages = defineMessages({
  stagingLabel: {
    id: 'test.environment.stagingLabel',
    defaultMessage: '!!!Staging vx',
    description: 'Label for staging network with version.'
  },
  testnetLabel: {
    id: 'test.environment.testnetLabel',
    defaultMessage: '!!!Testnet vx',
    description: 'Label for testnet with version.'
  },
});

type Props = {
  network: string,
};

export default class WalletTestEnvironmentLabel extends Component<Props> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { network } = this.props;
    const { intl } = this.context;
    const label = network === 'staging' ? 'stagingLabel' : 'testnetLabel';
    return (
      <div className={styles.component}>
        {intl.formatMessage(messages[label])}
      </div>
    );
  }

}
