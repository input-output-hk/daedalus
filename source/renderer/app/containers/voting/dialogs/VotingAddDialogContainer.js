// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { observer, inject } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { find, get } from 'lodash';
import BigNumber from 'bignumber.js';
import VotingAddDialog from '../../../components/voting/VotingAddDialog';
import VotingAddWizard from '../../../components/voting/voting-add-wizard/VotingAddWizard';
import {
  MIN_VOTING_FUNDS,
  VOTING_FEE_FOR_CALCULATE,
} from '../../../config/votingConfig';
import { FormattedHTMLMessageWithLink } from '../../../components/widgets/FormattedHTMLMessageWithLink';
import { formattedAmountToLovelace } from '../../../utils/formatters';
import type { InjectedDialogContainerProps } from '../../../types/injectedPropsType';

const messages = defineMessages({
  votingAddStep1Label: {
    id: 'voting.votingAdd.steps.step.1.label',
    defaultMessage: '!!!Wallet',
    description: 'Step 1 label text on voting add.',
  },
  votingAddStep2Label: {
    id: 'voting.votingAdd.steps.step.2.label',
    defaultMessage: '!!!Sign',
    description: 'Step 2 label text on voting add.',
  },
  votingAddStep3Label: {
    id: 'voting.votingAdd.steps.step.3.label',
    defaultMessage: '!!!Confirm',
    description: 'Step 3 label text on voting add.',
  },
  votingAddStep4Label: {
    id: 'voting.votingAdd.steps.step.4.label',
    defaultMessage: '!!!PIN code',
    description: 'Step 4 label text on voting add.',
  },
  votingAddStep5Label: {
    id: 'voting.votingAdd.steps.step.5.label',
    defaultMessage: 'QR code',
    description: 'Step 5 label text on voting add.',
  },
});

type Props = InjectedDialogContainerProps;

type State = {
  activeStep: number,
  selectedWalletId: string,
  transactionFee: BigNumber,
  transactionFeeError: string | Node | null,
};

@inject('stores', 'actions')
@observer
export default class VotingAddDialogContainer extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => {},
  };

  // We need to track the mounted state in order to avoid calling
  // setState promise handling code after the component was already unmounted:
  // Read more: https://facebook.github.io/react/blog/2015/12/16/ismounted-antipattern.html
  _isMounted = false;

  componentDidMount() {
    this._isMounted = true;
  }

  componentWillUnmount() {
    this._isMounted = false;
    this.props.stores.voting.votingSendTransactionRequest.reset();
    this.props.actions.voting.resetVotingRegistration.trigger();
  }

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
    transactionFeeError: null,
  };

  STEPS_LIST = [
    this.context.intl.formatMessage(messages.votingAddStep1Label),
    this.context.intl.formatMessage(messages.votingAddStep2Label),
    this.context.intl.formatMessage(messages.votingAddStep3Label),
    this.context.intl.formatMessage(messages.votingAddStep4Label),
    this.context.intl.formatMessage(messages.votingAddStep5Label),
  ];

  handleRollBack = () => {
    this.props.stores.voting.votingSendTransactionRequest.reset();
    this.props.actions.voting.resetVotingRegistration.trigger();
    this.setState({ activeStep: 1 });
  };

  handleContinue = () => {
    const { activeStep } = this.state;
    this.setState({ activeStep: activeStep + 1 });
  };

  handleSelectWallet = (walletId: string) => {
    this.setState({ selectedWalletId: walletId });
    this.props.actions.voting.selectVotingWallet.trigger(walletId);
    this._handleCalculateTransactionFee();
    this.handleContinue();
  };

  handleSetPinCode = (code: number) => {
    this.props.actions.voting.generateQrCode.trigger(code);
    this.handleContinue();
  };

  handleDeposit = (spendingPassword: string) => {
    const amount = formattedAmountToLovelace(`${VOTING_FEE_FOR_CALCULATE}`);

    this.props.stores.voting.votingSendTransactionRequest.reset();
    this.props.actions.voting.sendTransaction.trigger({
      passphrase: spendingPassword,
      amount,
    });
    this.props.actions.voting.initializeCountdownInterval.trigger();

    this.handleContinue();
  };

  render() {
    const {
      activeStep,
      selectedWalletId,
      transactionFee,
      transactionFeeError,
    } = this.state;
    const { wallets, staking, voting } = this.props.stores;

    const { stakePools, getStakePoolById } = staking;

    const {
      votingSendTransactionRequest,
      isVotingRegistrationTransactionPending,
      qrCode,
      countdownRemaining,
    } = voting;

    const selectedWallet = find(
      wallets.allWallets,
      (wallet) => wallet.id === selectedWalletId
    );

    return (
      <VotingAddDialog
        onClose={this.props.onClose}
        stepsList={this.STEPS_LIST}
        activeStep={activeStep}
      >
        <VotingAddWizard
          stakePoolsList={stakePools}
          wallets={wallets.allWallets}
          activeStep={activeStep}
          minVotingFunds={MIN_VOTING_FUNDS}
          isWalletAcceptable={this.handleIsWalletAcceptable}
          selectedWallet={selectedWallet}
          getStakePoolById={getStakePoolById}
          onContinue={this.handleContinue}
          onSelectWallet={this.handleSelectWallet}
          onSetPinCode={this.handleSetPinCode}
          onSubmit={this.handleDeposit}
          onRollback={this.handleRollBack}
          transactionFee={transactionFee}
          transactionFeeError={transactionFeeError}
          qrCode={qrCode}
          isSubmitting={
            votingSendTransactionRequest.isExecuting ||
            isVotingRegistrationTransactionPending
          }
          transactionError={votingSendTransactionRequest.error}
          countdownRemaining={countdownRemaining}
        />
      </VotingAddDialog>
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
      if (this._isMounted) {
        this.setState({
          transactionFee: fee,
          transactionFeeError: null,
        });
      }
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

      if (this._isMounted) {
        this.setState({
          transactionFee: new BigNumber(0),
          transactionFeeError,
        });
      }
    }
  }
}
