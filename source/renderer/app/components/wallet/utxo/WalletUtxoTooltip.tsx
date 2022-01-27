import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import { get } from 'lodash';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletUtxoTooltip.scss' or i... Remove this comment to see the full error message
import styles from './WalletUtxoTooltip.scss';
import { PRETTY_WALLET_AMOUNTS } from '../../../config/utxoConfig';

export const messages = defineMessages({
  tooltipFirst: {
    id: 'wallet.settings.utxos.tooltipFirst',
    defaultMessage:
      '!!!<b>{walletUtxosAmount}</b> UTXOs containing <br /> <b>{walletAmount}</b> ADA',
    description: 'Tooltip for the "Wallet Utxos - first bar" screen.',
  },
  tooltip: {
    id: 'wallet.settings.utxos.tooltip',
    defaultMessage:
      '!!!<b>{walletUtxosAmount}</b> UTXOs containing <br /> <span> between <b>{previousWalletAmount}</b> and </span> <b>{walletAmount}</b> ADA',
    description: 'Tooltip for the "Wallet Utxos" screen.',
  },
  tooltipLast: {
    id: 'wallet.settings.utxos.tooltipLast',
    defaultMessage:
      '!!!<b>{walletUtxosAmount}</b> UTXOs containing <br /> <b>{walletAmount}</b> ADA',
    description: 'Tooltip for the "Wallet Utxos - last bar" screen.',
  },
});
type Props = {
  label?: string;
  payload?: Array<{
    payload: {
      walletUtxosAmount: number;
    };
  }>;
};

@observer
class WalletUtxoTooltip extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  getPreviousAmount = (walletAmount: string) => {
    const walletAmountIndex = PRETTY_WALLET_AMOUNTS.findIndex(
      (wa) => wa === walletAmount
    );
    return PRETTY_WALLET_AMOUNTS[walletAmountIndex - 1];
  };

  render() {
    const { label: walletAmount = '', payload } = this.props;
    const { walletUtxosAmount } = get(payload, '[0].payload', {});
    const previousWalletAmount = this.getPreviousAmount(walletAmount);
    let message = messages.tooltip;
    if (!previousWalletAmount) message = messages.tooltipFirst;
    if (walletAmount === '10K+') message = messages.tooltipLast;
    return (
      <div className={styles.component}>
        <p>
          <FormattedHTMLMessage
            {...message}
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

export default WalletUtxoTooltip;
