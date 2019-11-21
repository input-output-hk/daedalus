// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import classNames from 'classnames';
import styles from './SyncingConnectingTitle.scss';
import { THEMES } from '../../../themes';

const messages = defineMessages({
  title: {
    id: 'loading.screen.syncingConnectingTitle',
    defaultMessage: '!!!Daedalus',
    description: 'Daedalus',
  },
  incentivizedTestnet: {
    id: 'loading.screen.syncingConnectingIncentivizedTestnet',
    defaultMessage: '!!!Incentivized Testnet',
    description: 'Incentivized Testnet',
  },
  balanceCheck: {
    id: 'loading.screen.syncingConnectingBalanceCheck',
    defaultMessage: '!!!Balance Check',
    description: 'Balance Check',
  },
});

type Props = {
  currentTheme: string,
  isIncentivizedTestnet: boolean,
};

@observer
export default class SyncingConnectingTitle extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { isIncentivizedTestnet, currentTheme } = this.props;
    const title = intl.formatMessage(messages.title);
    const subTitle1 = isIncentivizedTestnet
      ? intl.formatMessage(messages.incentivizedTestnet)
      : null;
    const subTitle2 = isIncentivizedTestnet
      ? intl.formatMessage(messages.balanceCheck)
      : null;
    const titleStyles = classNames([styles.textContent, styles.title]);
    const subTitle1Styles = classNames([styles.textContent, styles.subTitle1]);
    const subTitle2Styles = classNames([styles.textContent, styles.subTitle2]);

    return (
      <div className={styles.component}>
        {currentTheme === THEMES.INCENTIVIZED_TESTNET &&
        <div className={titleStyles}>{title}</div>}
        <div className={subTitle1Styles}>{subTitle1}</div>
        <div className={subTitle2Styles}>{subTitle2}</div>
      </div>
    );
  }
}
