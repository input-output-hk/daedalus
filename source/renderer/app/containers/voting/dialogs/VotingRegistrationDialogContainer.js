// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { observer, inject } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { find, get } from 'lodash';
import BigNumber from 'bignumber.js';
import VotingRegistrationDialog from '../../../components/voting/VotingRegistrationDialog';
import VotingRegistrationWizard from '../../../components/voting/voting-registration-wizard/VotingRegistrationWizard';
import {
  MIN_VOTING_REGISTRATION_FUNDS,
  VOTING_REGISTRATION_FEE_CALCULATION_AMOUNT,
} from '../../../config/votingConfig';
import { FormattedHTMLMessageWithLink } from '../../../components/widgets/FormattedHTMLMessageWithLink';
import { formattedAmountToLovelace } from '../../../utils/formatters';
import type { InjectedDialogContainerProps } from '../../../types/injectedPropsType';

const messages = defineMessages({
  votingRegistrationStep1Label: {
    id: 'voting.votingRegistration.steps.step.1.label',
    defaultMessage: '!!!Wallet',
    description: 'Step 1 label text on voting registration.',
  },
  votingRegistrationStep2Label: {
    id: 'voting.votingRegistration.steps.step.2.label',
    defaultMessage: '!!!Sign',
    description: 'Step 2 label text on voting registration.',
  },
  votingRegistrationStep3Label: {
    id: 'voting.votingRegistration.steps.step.3.label',
    defaultMessage: '!!!Confirm',
    description: 'Step 3 label text on voting registration.',
  },
  votingRegistrationStep4Label: {
    id: 'voting.votingRegistration.steps.step.4.label',
    defaultMessage: '!!!PIN code',
    description: 'Step 4 label text on voting registration.',
  },
  votingRegistrationStep5Label: {
    id: 'voting.votingRegistration.steps.step.5.label',
    defaultMessage: 'QR code',
    description: 'Step 5 label text on voting registration.',
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
export default class VotingRegistrationDialogContainer extends Component<
  Props,
  State
> {
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
    this.props.stores.voting.signMetadataRequest.reset();
    this.props.actions.voting.resetVotingRegistration.trigger();
  }

  handleIsWalletAcceptable = (
    walletAmount?: BigNumber,
    walletReward?: BigNumber = 0
  ) =>
    walletAmount &&
    walletAmount.gte(new BigNumber(MIN_VOTING_REGISTRATION_FUNDS)) &&
    !walletAmount.isEqualTo(walletReward);

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
    this.context.intl.formatMessage(messages.votingRegistrationStep1Label),
    this.context.intl.formatMessage(messages.votingRegistrationStep2Label),
    this.context.intl.formatMessage(messages.votingRegistrationStep3Label),
    this.context.intl.formatMessage(messages.votingRegistrationStep4Label),
    this.context.intl.formatMessage(messages.votingRegistrationStep5Label),
  ];

  handleRollBack = () => {
    this.props.stores.voting.votingSendTransactionRequest.reset();
    this.props.stores.voting.signMetadataRequest.reset();
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
    const amount = formattedAmountToLovelace(
      `${VOTING_REGISTRATION_FEE_CALCULATION_AMOUNT}`
    );
    this.props.stores.voting.votingSendTransactionRequest.reset();
    this.props.actions.voting.sendTransaction.trigger({
      amount,
      passphrase: spendingPassword,
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
    const { wallets, staking, voting, app } = this.props.stores;

    const { stakePools, getStakePoolById } = staking;

    const {
      votingSendTransactionRequest,
      signMetadataRequest,
      isVotingRegistrationTransactionPending,
      isVotingRegistrationTransactionApproved,
      qrCode,
      countdownRemaining,
    } = voting;

    const { openExternalLink } = app;

    const selectedWallet = find(
      wallets.allWallets,
      (wallet) => wallet.id === selectedWalletId
    );

    return (
      <VotingRegistrationDialog
        onClose={this.props.onClose}
        stepsList={this.STEPS_LIST}
        activeStep={activeStep}
      >
        <VotingRegistrationWizard
          stakePoolsList={stakePools}
          wallets={wallets.allWallets}
          activeStep={activeStep}
          minVotingRegistrationFunds={MIN_VOTING_REGISTRATION_FUNDS}
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
          isTransactionApproved={isVotingRegistrationTransactionApproved}
          transactionError={
            votingSendTransactionRequest.error
              ? votingSendTransactionRequest.error
              : signMetadataRequest.error
          }
          countdownRemaining={countdownRemaining}
          onExternalLinkClick={openExternalLink}
        />
      </VotingRegistrationDialog>
    );
  }

  async _handleCalculateTransactionFee() {
    const { transactions, addresses, app } = this.props.stores;
    const { calculateTransactionFee } = transactions;
    const { getAddressesByWalletId } = addresses;
    const amount = formattedAmountToLovelace(
      `${VOTING_REGISTRATION_FEE_CALCULATION_AMOUNT}`
    );
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
