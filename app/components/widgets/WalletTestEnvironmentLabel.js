import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import styles from './WalletTestEnvironmentLabel.scss';

const messages = defineMessages({
  testnetLabel: {
    id: 'test.environment.testnetLabel',
    defaultMessage: '!!!Testnet vx',
    description: 'Label for testnet with version.'
  },
});

export default class WalletTestEnvironmentLabel extends Component {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    return (
      <div className={styles.component}>
        {intl.formatMessage(messages.testnetLabel)}
      </div>
    );
  }

}
