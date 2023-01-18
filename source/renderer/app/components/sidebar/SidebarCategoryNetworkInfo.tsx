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
  vasil_dev: {
    id: 'test.environment.vasilDevLabel',
    defaultMessage: '!!!Vasil-Dev',
    description: 'Label for vasil_dev with version.',
  },
  preprod: {
    id: 'test.environment.preprodLabel',
    defaultMessage: '!!!Pre-Prod',
    description: 'Label for preprod with version.',
  },
  preview: {
    id: 'test.environment.previewLabel',
    defaultMessage: '!!!Preview',
    description: 'Label for preview with version.',
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
export default class SidebarCategoryNetworkInfo extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { isFlight } = global;
    const { network } = this.props;
    const { intl } = this.context;
    const label = messages[isFlight ? 'flight' : network];
    return <div className={styles.component}>{intl.formatMessage(label)}</div>;
  }
}
