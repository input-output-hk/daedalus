import React, { Component } from 'react';
// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'Node'.
import type { Node } from 'react';
import { observer, inject } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { find, get } from 'lodash';
import BigNumber from 'bignumber.js';
import {
  VOTING_REGISTRATION_MIN_WALLET_FUNDS,
  VOTING_REGISTRATION_FEE_CALCULATION_AMOUNT,
} from '../../../config/votingConfig';
import VotingRegistrationDialogWizard from '../../../components/voting/VotingRegistrationDialogWizard';
import ConfirmationDialog from '../../../components/voting/voting-registration-wizard-steps/widgets/ConfirmationDialog';
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
    defaultMessage: '!!!Register',
    description: 'Step 2 label text on voting registration.',
  },
  votingRegistrationStep3Label: {
    id: 'voting.votingRegistration.steps.step.3.label',
    defaultMessage: '!!!Confirm',
    description: 'Step 3 label text on voting registration.',
  },
  votingRegistrationStep4Label: {
    id: 'voting.votingRegistration.steps.step.4.label',
    defaultMessage: '!!!PIN',
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
  selectedWalletId: string;
  transactionFee: BigNumber;
  transactionFeeError: string | Node | null;
};

@inject('stores', 'actions')
@observer
class VotingRegistrationDialogContainer extends Component<Props, State> {
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
    this.props.actions.voting.resetRegistration.trigger();
  }

  handleIsWalletAcceptable = (
    isLegacy?: boolean,
    isRestoring?: boolean,
    walletAmount?: BigNumber,
    // @ts-ignore ts-migrate(2322) FIXME: Type 'number' is not assignable to type 'BigNumber... Remove this comment to see the full error message
    walletReward: BigNumber = 0
  ) =>
    !isLegacy &&
    !isRestoring &&
    walletAmount &&
    walletAmount.gte(new BigNumber(VOTING_REGISTRATION_MIN_WALLET_FUNDS)) &&
    !walletAmount.isEqualTo(walletReward);

  get selectedWalletId() {
    return get(this.props, ['stores', 'voting', 'selectedWalletId'], null);
  }

  state = {
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
  handleClose = (showConfirmationDialog?: boolean) => {
    if (showConfirmationDialog) {
      this.props.actions.voting.showConfirmationDialog.trigger();
    } else {
      this.props.actions.dialogs.closeActiveDialog.trigger();
    }
  };
  handleRestart = () => {
    this.props.actions.voting.resetRegistration.trigger();
    this.props.stores.hardwareWallets.sendMoneyRequest.reset();
  };
  handleContinue = () => {
    this.props.actions.voting.nextRegistrationStep.trigger();
  };
  handleBack = () => {
    this.props.actions.voting.previousRegistrationStep.trigger();
  };
  handleSelectWallet = (walletId: string) => {
    this.setState({
      selectedWalletId: walletId,
    });
    this.props.actions.voting.selectWallet.trigger(walletId);

    this._handleCalculateTransactionFee();

    this.handleContinue();
  };
  handleSetPinCode = (code: number) => {
    this.props.actions.voting.generateQrCode.trigger(code);
  };
  handleSendTransaction = (spendingPassword: string | null | undefined) => {
    const amount = formattedAmountToLovelace(
      `${VOTING_REGISTRATION_FEE_CALCULATION_AMOUNT}`
    );
    this.props.actions.voting.sendTransaction.trigger({
      amount,
      passphrase: spendingPassword,
    });
  };

  render() {
    const {
      selectedWalletId,
      transactionFee,
      transactionFeeError,
    } = this.state;
    const {
      wallets,
      staking,
      voting,
      app,
      hardwareWallets,
    } = this.props.stores;
    const { closeConfirmationDialog, saveAsPDF } = this.props.actions.voting;
    const { all } = wallets;
    const { stakePools, getStakePoolById } = staking;
    const {
      isConfirmationDialogOpen,
      registrationStep,
      getWalletPublicKeyRequest,
      createVotingRegistrationTransactionRequest,
      signMetadataRequest,
      isTransactionPending,
      isTransactionConfirmed,
      transactionConfirmations,
      qrCode,
    } = voting;
    const { openExternalLink } = app;
    const {
      hwDeviceStatus,
      checkIsTrezorByWalletId,
      sendMoneyRequest,
      isTransactionPending: isHwTransactionPending,
    } = hardwareWallets;
    const selectedWallet = find(
      all,
      (wallet) => wallet.id === selectedWalletId
    );
    let isTrezor = false;
    let isHardwareWallet = false;

    if (selectedWallet) {
      isTrezor = checkIsTrezorByWalletId(selectedWallet.id);
      isHardwareWallet = selectedWallet.isHardwareWallet;
    }

    return (
      <>
        <VotingRegistrationDialogWizard
          onClose={this.handleClose}
          stepsList={this.STEPS_LIST}
          activeStep={registrationStep}
          stakePoolsList={stakePools}
          wallets={all}
          minVotingRegistrationFunds={VOTING_REGISTRATION_MIN_WALLET_FUNDS}
          isWalletAcceptable={this.handleIsWalletAcceptable}
          selectedWallet={selectedWallet}
          getStakePoolById={getStakePoolById}
          onContinue={this.handleContinue}
          onSelectWallet={this.handleSelectWallet}
          onSetPinCode={this.handleSetPinCode}
          onSubmit={this.handleSendTransaction}
          onRestart={this.handleRestart}
          onBack={this.handleBack}
          transactionFee={transactionFee}
          transactionFeeError={transactionFeeError}
          qrCode={qrCode}
          onDownloadPDF={saveAsPDF.trigger}
          isTransactionPending={
            isTransactionPending || (isHardwareWallet && isHwTransactionPending)
          }
          isTransactionConfirmed={isTransactionConfirmed}
          transactionConfirmations={transactionConfirmations}
          hwDeviceStatus={hwDeviceStatus}
          transactionError={
            getWalletPublicKeyRequest.error ||
            createVotingRegistrationTransactionRequest.error ||
            signMetadataRequest.error ||
            (isHardwareWallet && sendMoneyRequest.error)
          }
          onExternalLinkClick={openExternalLink}
          isTrezor={isTrezor}
          isHardwareWallet={isHardwareWallet}
        />
        {isConfirmationDialogOpen && (
          <ConfirmationDialog
            onConfirm={closeConfirmationDialog.trigger}
            onCancel={() => {
              this.props.actions.dialogs.closeActiveDialog.trigger();
            }}
          />
        )}
      </>
    );
  }

  async _handleCalculateTransactionFee() {
    const {
      transactions,
      addresses,
      app,
      wallets,
      hardwareWallets,
      voting,
    } = this.props.stores;
    const { calculateTransactionFee } = transactions;
    const { getAddressesByWalletId } = addresses;
    const { getWalletById } = wallets;
    const { selectCoins, initiateTransaction } = hardwareWallets;
    const { prepareVotingData } = voting;
    const amount = formattedAmountToLovelace(
      `${VOTING_REGISTRATION_FEE_CALCULATION_AMOUNT}`
    );
    this.setState({
      transactionFee: null,
      transactionFeeError: null,
    });

    try {
      const selectedWallet = getWalletById(this.selectedWalletId);
      const [address] = await getAddressesByWalletId(this.selectedWalletId);
      const isHardwareWallet = get(selectedWallet, 'isHardwareWallet', false);
      let fee;
      let votingData;

      if (isHardwareWallet) {
        votingData = await prepareVotingData({
          walletId: this.selectedWalletId,
        });
        ({ fee } = await selectCoins({
          walletId: this.selectedWalletId,
          address: address.id,
          amount,
          metadata: votingData.metadata,
        }));
      } else {
        ({ fee } = await calculateTransactionFee({
          walletId: this.selectedWalletId,
          address: address.id,
          amount,
        }));
      }

      if (this._isMounted) {
        this.setState({
          transactionFee: fee,
          transactionFeeError: null,
        });
      }

      if (isHardwareWallet) {
        await initiateTransaction({
          walletId: this.selectedWalletId,
          votingData,
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

export default VotingRegistrationDialogContainer;
