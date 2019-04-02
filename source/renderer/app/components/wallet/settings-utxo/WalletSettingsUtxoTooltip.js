// @flow
import React, { Component } from 'react';
import classnames from 'classnames';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import { get } from 'lodash';
import styles from './WalletSettingsUtxoTooltip.scss';

export const messages = defineMessages({
  tooltip: {
    id: 'wallet.settings.utxos.tooltip',
    defaultMessage:
      '!!!<b>{walletUtxosAmount}</b> UTxOs containing <br /> <span> between <b>{prettyPreviousWalletAmount}</b> and </span> <b>{prettyWalletAmount}</b> ADA',
    description: 'Tooltip for the "Wallet Utxos" screen.',
  },
});

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
  static contextTypes = {
    intl: intlShape.isRequired,
  };

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
              prettyPreviousWalletAmount,
              prettyWalletAmount,
            }}
          />
        </p>
      </div>
    );
  }
}
