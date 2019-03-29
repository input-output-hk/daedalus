// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { get } from 'lodash';
import styles from './WalletSettingsUtxoTooltip.scss';

type Props = {
  payload?: Array<{
    payload: {
      walletAmount: number,
      walletUtxosAmount: number,
    },
  }>,
  getPrettyAmount: Function,
};

@observer
export default class WalletSettingsUtxoTooltip extends Component<Props> {
  getPreviousAmount = (walletAmount: number) => {
    if (walletAmount === 45000000000) return 10000000000;
    if (walletAmount === 0.00001) return null;
    return walletAmount / 10;
  };

  render() {
    const { getPrettyAmount, payload } = this.props;
    const { walletAmount, walletUtxosAmount } = get(payload, '[0].payload', {});
    const previousWalletAmount = this.getPreviousAmount(walletAmount);
    const prettyWalletAmount = getPrettyAmount(walletAmount);
    const prettyPreviousWalletAmount = getPrettyAmount(previousWalletAmount);
    return (
      <div className={styles.component}>
        <p>
          <b>{walletUtxosAmount}</b> UTxOs containing <br />
          {!!previousWalletAmount && (
            <span>
              between <b>${prettyPreviousWalletAmount}</b> and{' '}
            </span>
          )}
          <b>{prettyWalletAmount}</b> ADA
        </p>
      </div>
    );
  }
}
