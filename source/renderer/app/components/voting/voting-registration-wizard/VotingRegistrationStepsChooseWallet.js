// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import classNames from 'classnames';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import WalletsDropdown from '../../widgets/forms/WalletsDropdown';
import Wallet from '../../../domains/Wallet';
import commonStyles from './VotingRegistrationSteps.scss';
import styles from './VotingRegistrationStepsChooseWallet.scss';

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
      '!!!This wallet does not contain the minimum amount of {minVotingFunds} ADA that is required for voting to be enabled. Please select a wallet with <span>a minimum amount of {minVotingFunds} ADA</span> and click Continue.',
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
  errorRestoringWallet: {
    id: 'voting.votingRegistration.chooseWallet.step.errorRestoringWallet',
    defaultMessage:
      '!!!The wallet cannot be used for voting purposes while it is being synced with the blockchain.',
    description:
      'RestoringWallet Error Label on the voting registration "choose wallet" step.',
  },
  continueButtonLabel: {
    id: 'voting.votingRegistration.chooseWallet.step.continueButtonLabel',
    defaultMessage: '!!!Continue',
    description:
      'Label for continue button on the voting registration "choose wallet" step.',
  },
});

type Props = {
  numberOfStakePools: number,
  onSelectWallet: Function,
  onContinue: Function,
  wallets: Array<Wallet>,
  minVotingFunds: number,
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

  onSubmit = () => {
    this.props.onContinue();
    this.onSelectWallet();
  };

  render() {
    const { intl } = this.context;
    const { selectedWalletId } = this.state;
    const {
      wallets,
      minVotingFunds,
      isWalletAcceptable,
      numberOfStakePools,
      getStakePoolById,
    } = this.props;

    const buttonLabel = intl.formatMessage(messages.continueButtonLabel);

    const selectedWallet: ?Wallet = wallets.find(
      (wallet: Wallet) => wallet && wallet.id === selectedWalletId
    );

    const { amount, reward, isRestoring } = selectedWallet || {};

    let errorMessage;
    if (selectedWallet && !isWalletAcceptable(amount, reward)) {
      // Wallet is restoring
      if (isRestoring) errorMessage = messages.errorRestoringWallet;
      // Wallet only has Reward balance
      else if (!amount.isZero() && amount.isEqualTo(reward))
        errorMessage = messages.errorMinVotingFundsRewardsOnly;
      // Wallet balance < min delegation funds
      else errorMessage = messages.errorMinVotingFunds;
    }

    const error = errorMessage && (
      <p className={styles.errorMessage}>
        <FormattedHTMLMessage {...errorMessage} values={{ minVotingFunds }} />
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

    return (
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
        <Button
          label={buttonLabel}
          onClick={this.onSubmit}
          disabled={!selectedWalletId || !!error}
          skin={ButtonSkin}
        />
      </div>
    );
  }
}
