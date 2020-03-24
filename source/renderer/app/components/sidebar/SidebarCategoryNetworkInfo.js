// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import styles from './SidebarCategoryNetworkInfo.scss';
import type { networkType } from '../../types/networkTypes';

const messages = defineMessages({
  mainnet: {
    id: 'test.environment.mainnetLabel',
    defaultMessage: '!!!Mainnet vx',
    description: 'Label for mainnet network with version.',
  },
  staging: {
    id: 'test.environment.stagingLabel',
    defaultMessage: '!!!Staging vx',
    description: 'Label for staging network with version.',
  },
  testnet: {
    id: 'test.environment.testnetLabel',
    defaultMessage: '!!!Testnet vx',
    description: 'Label for testnet with version.',
  },
  development: {
    id: 'test.environment.developmentLabel',
    defaultMessage: '!!!Development vx',
    description: 'Label for development with version.',
  },
  nightly: {
    id: 'test.environment.nightlyLabel',
    defaultMessage: '!!!Nightly vx',
    description: 'Label for nightly with version.',
  },
  qa: {
    id: 'test.environment.qaLabel',
    defaultMessage: '!!!QA vx',
    description: 'Label for qa with version.',
  },
  selfnode: {
    id: 'test.environment.selfnodeLabel',
    defaultMessage: '!!!Selfnode vx',
    description: 'Label for selfnode with version.',
  },
  itn_selfnode: {
    id: 'test.environment.itnSelfnodeLabel',
    defaultMessage: '!!!Selfnode vx',
    description: 'Label for ITN selfnode with version.',
  },
  itn_rewards_v1: {
    id: 'test.environment.itnRewardsV1Label',
    defaultMessage: '!!!Development vx',
    description: 'Label for ITN Rewards V1 with version.',
  },
});

type Props = {
  network: networkType,
};

export default class SidebarCategoryNetworkInfo extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { network } = this.props;
    const { intl } = this.context;
    const label = messages[network];
    return <div className={styles.component}>{intl.formatMessage(label)}</div>;
  }
}
