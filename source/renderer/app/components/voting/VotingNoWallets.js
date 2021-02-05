// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import BigNumber from 'bignumber.js';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import styles from './VotingNoWallets.scss';
import icon from '../../assets/images/attention-big-thin.inline.svg';

const messages = defineMessages({
  headLine: {
    id: 'voting.info.noWallets.headLine',
    defaultMessage:
      '!!!Voting registration for Fund3 is not available as you currently do not have any Shelley-compatible wallets.',
    description: '"No wallets" headLine on the voting info page.',
  },
  instructions: {
    id: 'voting.info.noWallets.instructions',
    defaultMessage:
      '!!!Create a new wallet and transfer a minimum of {minVotingFunds} ADA (or restore an existing wallet with funds), then return here to register for voting.',
    description: '"No wallets" instructions on the voting info page.',
  },
  createWalletButtonLabel: {
    id: 'voting.info.noWallets.createWalletButtonLabel',
    defaultMessage: '!!!Create wallet',
    description:
      'Label for "Create New Wallet" button on the voting info page.',
  },
});

type Props = {
  onGoToCreateWalletClick: Function,
  minVotingFunds: number,
};

export default class VotingNoWallets extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { onGoToCreateWalletClick, minVotingFunds } = this.props;

    return (
      <div className={styles.component}>
        <SVGInline svg={icon} className={styles.icon} />
        <h1>{intl.formatMessage(messages.headLine)}</h1>
        <p>
          {intl.formatMessage(messages.instructions, {
            minVotingFunds: new BigNumber(minVotingFunds).toFormat(0),
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
