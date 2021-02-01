// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import classNames from 'classnames';
import WalletsDropdown from '../../widgets/forms/WalletsDropdown';
import Wallet from '../../../domains/Wallet';
import commonStyles from './VotingRegistrationSteps.scss';
import styles from './VotingRegistrationStepsChooseWallet.scss';
import VotingRegistrationDialog from './widgets/VotingRegistrationDialog';

const messages = defineMessages({
  description: {
    id: 'voting.votingRegistration.chooseWallet.step.description',
    defaultMessage:
      '!!!For  <span>fund 2</span> voting you will only be able to use one wallet to register.',
    description: 'Description on the voting registration "choose wallet" step.',
  },
  selectWalletInputLabel: {
    id: 'voting.votingRegistration.chooseWallet.step.selectWalletInputLabel',
    defaultMessage: '!!!Select wallet',
    description:
      'Label "Wallet" for select input on the voting registration "choose wallet" step.',
  },
  selectWalletInputPlaceholder: {
    id:
      'voting.votingRegistration.chooseWallet.step.selectWalletInputPlaceholder',
    defaultMessage: '!!!Select Wallet',
    description:
      'Placeholder "Select Wallet" for select input on the voting registration "choose wallet" step.',
  },
  errorMinVotingFunds: {
    id: 'voting.votingRegistration.chooseWallet.step.errorMinVotingFunds',
    defaultMessage:
      '!!!This wallet does not contain the minimum amount of {minVotingRegistrationFunds} ADA that is required for voting to be enabled. Please select a wallet with <span>a minimum amount of {minVotingRegistrationFunds} ADA</span> and click Continue.',
    description:
      'errorMinVotingFunds Error Label on the voting registration "choose wallet" step.',
  },
  errorMinVotingFundsRewardsOnly: {
    id:
      'voting.votingRegistration.chooseWallet.step.errorMinVotingFundsRewardsOnly',
    defaultMessage:
      '!!!This wallet contains rewards balances only, and cannot be used for voting purposes.',
    description:
      'errorMinVotingFundsRewardsOnly Error Label on the voting registration "choose wallet" step.',
  },
  errorLegacyWallet: {
    id: 'voting.votingRegistration.chooseWallet.step.errorLegacyWallet',
    defaultMessage:
      '!!!This wallet cannot be registered for voting because it is a legacy Byron wallet.',
    description:
      'Byron wallet error message on the voting registration "choose wallet" step.',
  },
  errorHardwareWallet: {
    id: 'voting.votingRegistration.chooseWallet.step.errorHardwareWallet',
    defaultMessage:
      '!!!This wallet cannot be registered for voting because it is a hardware wallet. <span>Hardware wallets will be supported in the future.</span>',
    description:
      'Hardware wallet error message on the voting registration "choose wallet" step.',
  },
  errorRestoringWallet: {
    id: 'voting.votingRegistration.chooseWallet.step.errorRestoringWallet',
    defaultMessage:
      '!!!The wallet cannot be used for voting purposes while it is being synced with the blockchain.',
    description:
      'Restoring wallet error message on the voting registration "choose wallet" step.',
  },
  continueButtonLabel: {
    id: 'voting.votingRegistration.chooseWallet.step.continueButtonLabel',
    defaultMessage: '!!!Continue',
    description:
      'Label for continue button on the voting registration "choose wallet" step.',
  },
});

type Props = {
  onClose: Function,
  stepsList: Array<string>,
  activeStep: number,
  numberOfStakePools: number,
  onSelectWallet: Function,
  wallets: Array<Wallet>,
  minVotingRegistrationFunds: number,
  selectedWalletId: ?string,
  isWalletAcceptable: Function,
  getStakePoolById: Function,
};

type State = {
  selectedWalletId: ?string,
};

export default class VotingRegistrationStepsChooseWallet extends Component<
  Props,
  State
> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    selectedWalletId: this.props.selectedWalletId,
  };

  onWalletChange = (selectedWalletId: string) => {
    this.setState({ selectedWalletId });
  };

  onSelectWallet = () => {
    const { selectedWalletId } = this.state;
    this.props.onSelectWallet(selectedWalletId);
  };

  render() {
    const { intl } = this.context;
    const { selectedWalletId } = this.state;
    const {
      onClose,
      stepsList,
      activeStep,
      wallets,
      minVotingRegistrationFunds,
      isWalletAcceptable,
      numberOfStakePools,
      getStakePoolById,
    } = this.props;

    const buttonLabel = intl.formatMessage(messages.continueButtonLabel);

    const selectedWallet: ?Wallet = wallets.find(
      (wallet: Wallet) => wallet && wallet.id === selectedWalletId
    );

    const { amount, reward, isLegacy, isHardwareWallet, isRestoring } =
      selectedWallet || {};

    let errorMessage;
    if (
      selectedWallet &&
      !isWalletAcceptable(isLegacy, isHardwareWallet, amount, reward)
    ) {
      // Wallet is a legacy wallet
      if (isLegacy) errorMessage = messages.errorLegacyWallet;
      // Wallet is a hardware wallet
      else if (isHardwareWallet) errorMessage = messages.errorHardwareWallet;
      // Wallet is restoring
      else if (isRestoring) errorMessage = messages.errorRestoringWallet;
      // Wallet only has Reward balance
      else if (!amount.isZero() && amount.isEqualTo(reward))
        errorMessage = messages.errorMinVotingFundsRewardsOnly;
      // Wallet balance < min voting registration funds
      else errorMessage = messages.errorMinVotingFunds;
    }

    const error = errorMessage && (
      <p className={styles.errorMessage}>
        <FormattedHTMLMessage
          {...errorMessage}
          values={{ minVotingRegistrationFunds }}
        />
      </p>
    );

    const className = classNames([
      commonStyles.votingRegistrationSteps,
      styles.votingRegistrationStepsChooseWalletWrapper,
    ]);

    const contentClassName = classNames([commonStyles.content, styles.content]);

    const walletSelectClasses = classNames([
      styles.walletSelect,
      error ? styles.error : null,
    ]);

    const actions = [
      {
        label: buttonLabel,
        onClick: this.onSelectWallet,
        disabled: !selectedWalletId || !!error,
      },
    ];

    return (
      <VotingRegistrationDialog
        onClose={onClose}
        stepsList={stepsList}
        activeStep={activeStep}
        actions={actions}
      >
        <div className={className}>
          <div className={contentClassName}>
            <p className={styles.description}>
              <FormattedHTMLMessage {...messages.description} />
            </p>
            <WalletsDropdown
              className={walletSelectClasses}
              label={intl.formatMessage(messages.selectWalletInputLabel)}
              numberOfStakePools={numberOfStakePools}
              wallets={wallets}
              onChange={(walletId: string) => this.onWalletChange(walletId)}
              placeholder={intl.formatMessage(
                messages.selectWalletInputPlaceholder
              )}
              value={selectedWalletId}
              getStakePoolById={getStakePoolById}
            />
            {error}
          </div>
        </div>
      </VotingRegistrationDialog>
    );
  }
}
