// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { find, get, take } from 'lodash';
import BigNumber from 'bignumber.js';
import VotingAdd from '../../components/voting/VotingAdd';
import VotingAddWizard from '../../components/voting/voting-add-wizard/VotingAddWizard';
import {
  MIN_VOTING_FUNDS,
  VOTING_FEE_FOR_CALCULATE,
} from '../../config/votingConfig';
import type { Node } from 'react';
import { FormattedHTMLMessageWithLink } from '../../components/widgets/FormattedHTMLMessageWithLink';
import type { InjectedProps as Props } from '../../types/injectedPropsType';
import { formattedAmountToLovelace } from '../../utils/formatters';
import { ROUTES } from '../../routes-config';

const messages = defineMessages({
  votingAddStep1Label: {
    id: 'voting.votingAdd.steps.step.1.label',
    defaultMessage: 'Wallet',
    description: 'Step 1 label text on voting add.',
  },
  votingAddStep2Label: {
    id: 'voting.votingAdd.steps.step.2.label',
    defaultMessage: 'PIN code',
    description: 'Step 2 label text on voting add.',
  },
  votingAddStep3Label: {
    id: 'voting.votingAdd.steps.step.3.label',
    defaultMessage: 'Deposit',
    description: 'Step 3 label text on voting add.',
  },
  votingAddStep4Label: {
    id: 'voting.votingAdd.steps.step.4.label',
    defaultMessage: 'QR code',
    description: 'Step 4 label text on voting add.',
  },
});

type State = {
  activeStep: number,
  selectedWalletId: string,
  transactionFee: BigNumber,
  transactionFeeError: ?string | ?Node,
  isLoading: Boolean,
  pinCode: number,
};

@inject('stores', 'actions')
@observer
export default class VotingAddPage extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static defaultProps = {
    actions: null,
    stores: null,
  };

  handleIsWalletAcceptable = (
    walletAmount?: BigNumber,
    walletReward?: BigNumber = 0
  ) =>
    walletAmount &&
    walletAmount.gte(new BigNumber(MIN_VOTING_FUNDS)) &&
    !walletAmount.equals(walletReward);

  get selectedWalletId() {
    return get(
      this.props,
      ['stores', 'voting', 'selectedVotingWalletId'],
      null
    );
  }

  state = {
    activeStep: 1,
    selectedWalletId: this.selectedWalletId,
    transactionFee: null,
  };

  STEPS_LIST = [
    this.context.intl.formatMessage(messages.votingAddStep1Label),
    this.context.intl.formatMessage(messages.votingAddStep2Label),
    this.context.intl.formatMessage(messages.votingAddStep3Label),
    this.context.intl.formatMessage(messages.votingAddStep4Label),
  ];

  handleContinue = () => {
    const { activeStep } = this.state;
    this.setState({ activeStep: activeStep + 1 });
  };

  onClose = () => {
    this.props.stores.voting.votingSendTransactionRequest.reset();
    this.props.actions.router.goToRoute.trigger({ route: ROUTES.VOTING.INFO });
  };

  handleSelectWallet = (walletId: string) => {
    this.setState({ selectedWalletId: walletId });
    this.props.actions.voting.selectVotingWallet.trigger(walletId);
    this._handleCalculateTransactionFee();
    this.handleContinue();
  };

  handleSetPinCode = (code: number) => {
    this.setState({ pinCode: code });
    this.handleContinue();
  };

  handleDeposit = (spendingPassword: string) => {
    const amount = formattedAmountToLovelace(`${VOTING_FEE_FOR_CALCULATE}`);

    this.props.actions.voting.sendTransaction.trigger({
      passphrase: spendingPassword,
      amount,
    });
    this.handleContinue();
  };

  render() {
    const { activeStep, selectedWalletId, transactionFee } = this.state;
    const { wallets, staking, voting } = this.props.stores;

    const { stakePools, getStakePoolById } = staking;

    const { votingSendTransactionRequest } = voting;

    const selectedWallet = find(
      wallets.allWallets,
      (wallet) => wallet.id === selectedWalletId
    );

    const acceptableWallets = find(wallets.allWallets, ({ amount, reward }) =>
      this.handleIsWalletAcceptable(amount, reward)
    );

    return (
      <VotingAdd stepsList={this.STEPS_LIST} activeStep={activeStep}>
        <VotingAddWizard
          stakePoolsList={stakePools}
          wallets={wallets.allWallets}
          activeStep={activeStep}
          isDisabled={activeStep === 1 && !acceptableWallets}
          minVotingFunds={MIN_VOTING_FUNDS}
          isWalletAcceptable={this.handleIsWalletAcceptable}
          selectedWallet={selectedWallet}
          getStakePoolById={getStakePoolById}
          onContinue={this.handleContinue}
          onSelectWallet={this.handleSelectWallet}
          onSetPinCode={this.handleSetPinCode}
          onClose={this.onClose}
          onConfirm={this.handleDeposit}
          transactionFee={transactionFee}
          isSubmitting={votingSendTransactionRequest.isExecuting}
        />
      </VotingAdd>
    );
  }

  async _handleCalculateTransactionFee() {
    const { transactions, addresses, app } = this.props.stores;
    const { calculateTransactionFee } = transactions;
    const { getAddressesByWalletId } = addresses;
    const amount = formattedAmountToLovelace(`${VOTING_FEE_FOR_CALCULATE}`);
    this.setState({
      transactionFee: null,
      transactionFeeError: null,
    });
    try {
      const [address] = await getAddressesByWalletId(this.selectedWalletId);

      const fee = await calculateTransactionFee({
        walletId: this.selectedWalletId,
        address: address.id,
        amount,
      });
      this.setState({
        transactionFee: fee,
        transactionFeeError: null,
      });
    } catch (error) {
      const errorHasLink = !!get(error, ['values', 'linkLabel']);
      const transactionFeeError = errorHasLink ? (
        <FormattedHTMLMessageWithLink
          message={error}
          onExternalLinkClick={app.openExternalLink}
        />
      ) : (
        this.context.intl.formatMessage(error)
      );

      this.setState({
        transactionFee: new BigNumber(0),
        transactionFeeError,
      });
    }
  }
}
