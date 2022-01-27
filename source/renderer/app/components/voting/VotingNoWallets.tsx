import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import BigNumber from 'bignumber.js';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './VotingNoWallets.scss' or its... Remove this comment to see the full error message
import styles from './VotingNoWallets.scss';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../assets/images/attention-... Remove this comment to see the full error message
import icon from '../../assets/images/attention-big-thin.inline.svg';
import { NEXT_VOTING_FUND_NUMBER } from '../../config/votingConfig';

const messages = defineMessages({
  headLine: {
    id: 'voting.info.noWallets.headLine',
    defaultMessage:
      '!!!Voting registration for Fund{nextVotingFundNumber} is not available as you currently do not have any Shelley-compatible wallets.',
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
  onGoToCreateWalletClick: (...args: Array<any>) => any;
  minVotingFunds: number;
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
        <h1>
          {intl.formatMessage(messages.headLine, {
            nextVotingFundNumber: NEXT_VOTING_FUND_NUMBER,
          })}
        </h1>
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
