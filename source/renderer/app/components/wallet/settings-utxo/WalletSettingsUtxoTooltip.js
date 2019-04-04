// @flow
import React, { Component } from 'react';
import classnames from 'classnames';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import { get } from 'lodash';
import styles from './WalletSettingsUtxoTooltip.scss';
import { PRETTY_WALLET_AMOUNTS } from '../../../config/utxoConfig';

export const messages = defineMessages({
  tooltip: {
    id: 'wallet.settings.utxos.tooltip',
    defaultMessage:
      '!!!<b>{walletUtxosAmount}</b> UTxOs containing <br /> <span> between <b>{previousWalletAmount}</b> and </span> <b>{walletAmount}</b> ADA',
    description: 'Tooltip for the "Wallet Utxos" screen.',
  },
});

type Props = {
  label?: string,
  payload?: Array<{
    payload: {
      walletUtxosAmount: number,
    },
  }>,
};

@observer
export default class WalletSettingsUtxoTooltip extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  getPreviousAmount = (walletAmount: string) => {
    const walletAmountIndex = PRETTY_WALLET_AMOUNTS.findIndex(
      wa => wa === walletAmount
    );
    return PRETTY_WALLET_AMOUNTS[walletAmountIndex - 1];
  };

  render() {
    const { label: walletAmount = '', payload } = this.props;
    const { walletUtxosAmount } = get(payload, '[0].payload', {});
    const previousWalletAmount = this.getPreviousAmount(walletAmount);
    const componentStyles = classnames([
      styles.component,
      !previousWalletAmount ? styles.noPreviousWalletAmount : null,
    ]);

    return (
      <div className={componentStyles}>
        <p>
          <FormattedHTMLMessage
            {...messages.tooltip}
            values={{
              walletUtxosAmount,
              previousWalletAmount,
              walletAmount,
            }}
          />
        </p>
      </div>
    );
  }
}
