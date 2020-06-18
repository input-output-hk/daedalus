// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import styles from './DelegationCenterNoWallets.scss';
import icon from '../../../assets/images/attention-big-thin.inline.svg';

const messages = defineMessages({
  headLine: {
    id: 'staking.delegationCenter.noWallets.headLine',
    defaultMessage:
      '!!!The delegation center is not available because you currently do not have any Shelley-compatible wallets.',
    description: '"No wallets" headLine on the Delegation centre Page.',
  },
  instructions: {
    id: 'staking.delegationCenter.noWallets.instructions',
    defaultMessage:
      '!!!Create a new wallet and transfer in a minimum of {minDelegationFunds} ada (or restore an existing wallet with funds), then return here to delegate your stake.',
    description: '"No wallets" instructions on the Delegation centre Page.',
  },
  createWalletButtonLabel: {
    id: 'staking.delegationCenter.noWallets.createWalletButtonLabel',
    defaultMessage: '!!!Create a wallet',
    description:
      'Label for "Create New Wallet" button on the Delegation centre Page.',
  },
});

type Props = {
  onGoToCreateWalletClick: Function,
  minDelegationFunds: number,
};

export default class DelegationCenterNoWallets extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { onGoToCreateWalletClick, minDelegationFunds } = this.props;

    return (
      <div className={styles.component}>
        <SVGInline svg={icon} className={styles.icon} />
        <h1>{intl.formatMessage(messages.headLine)}</h1>
        <p>
          {intl.formatMessage(messages.instructions, { minDelegationFunds })}
        </p>
        <Button
          className="primary"
          onClick={onGoToCreateWalletClick}
          label={intl.formatMessage(messages.createWalletButtonLabel)}
          skin={ButtonSkin}
        />
      </div>
    );
  }
}
