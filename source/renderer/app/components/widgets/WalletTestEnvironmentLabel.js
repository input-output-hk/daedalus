import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import styles from './WalletTestEnvironmentLabel.scss';

const messages = defineMessages({
  staging: {
    id: 'test.environment.stagingLabel',
    defaultMessage: '!!!Staging vx',
    description: 'Label for staging network with version.'
  },
  testnet: {
    id: 'test.environment.testnetLabel',
    defaultMessage: '!!!Testnet vx',
    description: 'Label for testnet with version.'
  },
  development: {
    id: 'test.environment.developmentLabel',
    defaultMessage: '!!!Development vx',
    description: 'Label for development with version.'
  }
});

type Props = {
  network: string,
};

export default class WalletTestEnvironmentLabel extends Component<Props> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { network='development' } = this.props;
    const { intl } = this.context;
    const label = messages[network];
    return (
      <div className={styles.component}>
        {intl.formatMessage(label)}
      </div>
    );
  }

}
