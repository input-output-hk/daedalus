import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import BigNumber from 'bignumber.js';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './DelegationCenterNoWallets.sc... Remove this comment to see the full error message
import styles from './DelegationCenterNoWallets.scss';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/attenti... Remove this comment to see the full error message
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
      '!!!Create a new wallet and transfer in a minimum of {minDelegationFunds} ADA (or restore an existing wallet with funds), then return here to delegate your stake.',
    description: '"No wallets" instructions on the Delegation centre Page.',
  },
  createWalletButtonLabel: {
    id: 'staking.delegationCenter.noWallets.createWalletButtonLabel',
    defaultMessage: '!!!Create wallet',
    description:
      'Label for "Create New Wallet" button on the Delegation centre Page.',
  },
});
type Props = {
  onGoToCreateWalletClick: (...args: Array<any>) => any;
  minDelegationFunds: number;
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
          {intl.formatMessage(messages.instructions, {
            minDelegationFunds: new BigNumber(minDelegationFunds).toFormat(0),
          })}
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
