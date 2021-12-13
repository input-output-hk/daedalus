import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletTestEnvironmentLabel.s... Remove this comment to see the full error message
import styles from './WalletTestEnvironmentLabel.scss';
import type { networkType } from '../../types/networkTypes';

const messages = defineMessages({
  flight: {
    id: 'test.environment.daedalusFlightLabel',
    defaultMessage: '!!!Cardano mainnet - Daedalus Flight',
    description: 'Label for Daedalus Flight with version.',
  },
  testnet: {
    id: 'test.environment.testnetLabel',
    defaultMessage: '!!!Testnet vx',
    description: 'Label for testnet with version.',
  },
  staging: {
    id: 'test.environment.stagingLabel',
    defaultMessage: '!!!Staging vx',
    description: 'Label for staging network with version.',
  },
  shelley_qa: {
    id: 'test.environment.shelleyQaLabel',
    defaultMessage: '!!!Shelley QA',
    description: 'Label for shelley_qa with version.',
  },
  alonzo_purple: {
    id: 'test.environment.alonzoPurpleLabel',
    defaultMessage: '!!!Alonzo Purple',
    description: 'Label for alonzo_purple with version.',
  },
  selfnode: {
    id: 'test.environment.selfnodeLabel',
    defaultMessage: '!!!Selfnode vx',
    description: 'Label for selfnode with version.',
  },
  development: {
    id: 'test.environment.developmentLabel',
    defaultMessage: '!!!Development vx',
    description: 'Label for development with version.',
  },
});
type Props = {
  network: networkType;
};
export default class WalletTestEnvironmentLabel extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'isFlight' does not exist on type 'typeof... Remove this comment to see the full error message
    const { isFlight } = global;
    const { network } = this.props;
    const { intl } = this.context;
    const label = messages[isFlight ? 'flight' : network];
    return <div className={styles.component}>{intl.formatMessage(label)}</div>;
  }
}
