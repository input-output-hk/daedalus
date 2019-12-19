// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';


import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';

import styles from './DelegationCenterNoWallets.scss';
import icon from '../../../assets/images/attention-big-thin.inline.svg';

const messages = defineMessages({
  headLine: {
    id: 'staking.delegationCenter.noWallets.headLine',
    defaultMessage:
      "!!!The Delegation Center is not available because you don't have any wallets.",
    description: '"No wallets" headLine on the Delegation centre Page.',
  },
  instructions: {
    id: 'staking.delegationCenter.noWallets.instructions',
    defaultMessage:
      '!!!Create a new wallet (or restore an existing one) then come back here to delegate your stake.',
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
  onGoToCreateWalletClick: Function,
};

export default class DelegationCenterNoWallets extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { onGoToCreateWalletClick } = this.props;

    return (
      <div className={styles.component}>
        <SVGInline svg={icon} className={styles.icon} />
        <h1>{intl.formatMessage(messages.headLine)}</h1>
        <p>{intl.formatMessage(messages.instructions)}</p>
        <Link
          onClick={() => {console.debug('CLICK')}}
          label="HELLO link"
          isUnderlined
          skin={LinkSkin}
        />
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
