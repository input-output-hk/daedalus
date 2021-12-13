import React, { Component } from 'react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import BigNumber from 'bignumber.js';
import classNames from 'classnames';
import WalletsDropdown from '../../widgets/forms/WalletsDropdown';
import Wallet from '../../../domains/Wallet';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './VotingRegistrationStepsChoos... Remove this comment to see the full error message
import styles from './VotingRegistrationStepsChooseWallet.scss';
import VotingRegistrationDialog from './widgets/VotingRegistrationDialog';

const messages = defineMessages({
  description: {
    id: 'voting.votingRegistration.chooseWallet.step.description',
    defaultMessage:
      '!!!You can only use one wallet when registering. To maximize rewards and voting power, choose the wallet with the largest balance.',
    description: 'Description on the voting registration "choose wallet" step.',
  },
  selectWalletInputLabel: {
    id: 'voting.votingRegistration.chooseWallet.step.selectWalletInputLabel',
    defaultMessage: '!!!Select a wallet',
    description:
      'Label "Wallet" for select input on the voting registration "choose wallet" step.',
  },
  selectWalletInputPlaceholder: {
    id:
      'voting.votingRegistration.chooseWallet.step.selectWalletInputPlaceholder',
    defaultMessage: '!!!Select a wallet',
    description:
      'Placeholder "Select Wallet" for select input on the voting registration "choose wallet" step.',
  },
  errorMinVotingFunds: {
    id: 'voting.votingRegistration.chooseWallet.step.errorMinVotingFunds',
    defaultMessage:
      '!!!This wallet does not contain the minimum required amount of <span>{minVotingRegistrationFunds} ADA</span>. Please select a different wallet with <span>a minimum balance of {minVotingRegistrationFunds} ADA</span>.',
    description:
      'errorMinVotingFunds Error Label on the voting registration "choose wallet" step.',
  },
  errorMinVotingFundsRewardsOnly: {
    id:
      'voting.votingRegistration.chooseWallet.step.errorMinVotingFundsRewardsOnly',
    defaultMessage:
      '!!!This wallet cannot be registered for voting as it contains rewards balance only.',
    description:
      'errorMinVotingFundsRewardsOnly Error Label on the voting registration "choose wallet" step.',
  },
  errorLegacyWallet: {
    id: 'voting.votingRegistration.chooseWallet.step.errorLegacyWallet',
    defaultMessage:
      '!!!This wallet cannot be registered for voting as it is a legacy Byron wallet.',
    description:
      'Byron wallet error message on the voting registration "choose wallet" step.',
  },
  errorRestoringWallet: {
    id: 'voting.votingRegistration.chooseWallet.step.errorRestoringWallet',
    defaultMessage:
      '!!!The wallet cannot be registered for voting while it is being synced with the blockchain.',
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
  onClose: (...args: Array<any>) => any;
  stepsList: Array<string>;
  activeStep: number;
  numberOfStakePools: number;
  onSelectWallet: (...args: Array<any>) => any;
  wallets: Array<Wallet>;
  minVotingRegistrationFunds: number;
  selectedWalletId: string | null | undefined;
  isWalletAcceptable: (...args: Array<any>) => any;
  getStakePoolById: (...args: Array<any>) => any;
};
type State = {
  selectedWalletId: string | null | undefined;
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
    this.setState({
      selectedWalletId,
    });
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
    const selectedWallet: Wallet | null | undefined = wallets.find(
      (wallet: Wallet) => wallet && wallet.id === selectedWalletId
    );
    const { amount, reward, isLegacy, isRestoring } = selectedWallet || {};
    let errorMessage;

    if (
      selectedWallet &&
      !isWalletAcceptable(isLegacy, isRestoring, amount, reward)
    ) {
      // Wallet is a legacy wallet
      if (isLegacy) errorMessage = messages.errorLegacyWallet;
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
          values={{
            minVotingRegistrationFunds: new BigNumber(
              minVotingRegistrationFunds
            ).toFormat(0),
          }}
        />
      </p>
    );
    const walletSelectClasses = classNames([
      styles.walletSelect,
      error ? styles.error : null,
    ]);
    const actions = [
      {
        label: buttonLabel,
        onClick: this.onSelectWallet,
        disabled: !selectedWalletId || !!error,
        primary: true,
      },
    ];
    return (
      <VotingRegistrationDialog
        onClose={() => {
          onClose();
        }}
        stepsList={stepsList}
        activeStep={activeStep}
        actions={actions}
        containerClassName={styles.component}
      >
        <p className={styles.description}>
          <FormattedHTMLMessage {...messages.description} />
        </p>
        <WalletsDropdown
          className={walletSelectClasses}
          // @ts-ignore ts-migrate(2322) FIXME: Type '{ className: any; label: any; numberOfStakeP... Remove this comment to see the full error message
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
      </VotingRegistrationDialog>
    );
  }
}
