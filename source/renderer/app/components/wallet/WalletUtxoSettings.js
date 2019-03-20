// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import styles from './WalletUtxoSettings.scss';
import type { WalletUtxos } from '../../api/wallets/types';

export const messages = defineMessages({});

type Props = {
  walletUtxos: ?WalletUtxos,
};

@observer
export default class WalletUtxoSettings extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { walletUtxos } = this.props;

    if (!walletUtxos) return <div className={styles.component}>Loading...</div>;

    const { allStakes, histogram, boundType } = walletUtxos;
    return (
      <div className={styles.component}>
        <pre>
          {JSON.stringify({ allStakes, histogram, boundType }, null, 2)}
        </pre>
      </div>
    );
  }
}
