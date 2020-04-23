// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import classNames from 'classnames';
import styles from './SyncingConnectingTitle.scss';

const messages = defineMessages({
  title: {
    id: 'loading.screen.syncingConnectingTitle',
    defaultMessage: '!!!Daedalus',
    description: 'Daedalus',
  },
  incentivizedTestnet: {
    id: 'loading.screen.syncingConnectingIncentivizedTestnet',
    defaultMessage: '!!!INCENTIVIZED TESTNET v1',
    description: 'INCENTIVIZED TESTNET v1',
  },
  balanceCheck: {
    id: 'loading.screen.syncingConnectingRewards',
    defaultMessage: '!!!Rewards',
    description: 'Rewards',
  },
});

@observer
export default class SyncingConnectingTitle extends Component<any> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const title = intl.formatMessage(messages.title);
    const subTitle1 = intl.formatMessage(messages.incentivizedTestnet);
    const subTitle2 = intl.formatMessage(messages.balanceCheck);
    const titleStyles = classNames([styles.textContent, styles.title]);
    const subTitle1Styles = classNames([styles.textContent, styles.subTitle1]);
    const subTitle2Styles = classNames([styles.textContent, styles.subTitle2]);

    return (
      <div className={styles.component}>
        <div className={titleStyles}>{title}</div>
        <div className={subTitle1Styles}>{subTitle1}</div>
        <div className={subTitle2Styles}>{subTitle2}</div>
      </div>
    );
  }
}
