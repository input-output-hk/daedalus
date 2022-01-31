import React, { Component } from 'react';
import {
  defineMessages,
  intlShape,
  FormattedHTMLMessage,
  FormattedMessage,
} from 'react-intl';
import classNames from 'classnames';
import { Stepper } from 'react-polymorph/lib/components/Stepper';
import { StepperSkin } from 'react-polymorph/lib/skins/simple/StepperSkin';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './DelegationSteps.scss' or its... Remove this comment to see the full error message
import commonStyles from './DelegationSteps.scss';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './DelegationStepsChooseWalletD... Remove this comment to see the full error message
import styles from './DelegationStepsChooseWalletDialog.scss';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import DialogBackButton from '../../widgets/DialogBackButton';
import Dialog from '../../widgets/Dialog';
import WalletsDropdown from '../../widgets/forms/WalletsDropdown';
import Wallet from '../../../domains/Wallet';

const messages = defineMessages({
  title: {
    id: 'staking.delegationSetup.chooseWallet.step.dialog.title',
    defaultMessage: '!!!Delegate wallet',
    description:
      'Title "Delegate wallet" on the delegation setup "choose wallet" step dialog.',
  },
  description: {
    id: 'staking.delegationSetup.chooseWallet.step.dialog.description',
    defaultMessage:
      '!!!Choose a wallet that holds the funds you want to delegate. The selected wallet must contain a <span>minimum amount of {minDelegationFunds} ADA</span> for delegation to be an option.',
    description:
      'Description on the delegation setup "choose wallet" step dialog.',
  },
  selectWalletInputLabel: {
    id:
      'staking.delegationSetup.chooseWallet.step.dialog.selectWalletInputLabel',
    defaultMessage: '!!!Wallet',
    description:
      'Label "Wallet" for select input on the delegation setup "choose wallet" step dialog.',
  },
  selectWalletInputPlaceholder: {
    id:
      'staking.delegationSetup.chooseWallet.step.dialog.selectWalletInputPlaceholder',
    defaultMessage: '!!!Select Wallet',
    description:
      'Placeholder "Select Wallet" for select input on the delegation setup "choose wallet" step dialog.',
  },
  stepIndicatorLabel: {
    id: 'staking.delegationSetup.chooseWallet.step.dialog.stepIndicatorLabel',
    defaultMessage: '!!!STEP {currentStep} OF {totalSteps}',
    description:
      'Step indicator label on the delegation setup "choose wallet" step dialog.',
  },
  errorMinDelegationFunds: {
    id:
      'staking.delegationSetup.chooseWallet.step.dialog.errorMinDelegationFunds',
    defaultMessage:
      '!!!This wallet does not contain the minimum amount of {minDelegationFunds} ADA which is required for delegation to be available. Please select a wallet with <span>a minimum amount of {minDelegationFunds} ADA</span> and click continue.',
    description:
      'errorMinDelegationFunds Error Label on the delegation setup "choose wallet" step dialog.',
  },
  errorMinDelegationFundsRewardsOnly: {
    id:
      'staking.delegationSetup.chooseWallet.step.dialog.errorMinDelegationFundsRewardsOnly',
    defaultMessage:
      '!!!This wallet contains only rewards balances so it cannot be delegated.',
    description:
      'errorMinDelegationFundsRewardsOnly Error Label on the delegation setup "choose wallet" step dialog.',
  },
  errorRestoringWallet: {
    id: 'staking.delegationSetup.chooseWallet.step.dialog.errorRestoringWallet',
    defaultMessage:
      '!!!This wallet can’t be used for delegation while it’s being synced.',
    description:
      'RestoringWallet Error Label on the delegation setup "choose wallet" step dialog.',
  },
  continueButtonLabel: {
    id: 'staking.delegationSetup.chooseWallet.step.dialog.continueButtonLabel',
    defaultMessage: '!!!Continue',
    description:
      'Label for continue button on the delegation setup "choose wallet" step dialog.',
  },
});
type Props = {
  numberOfStakePools: number;
  onClose: (...args: Array<any>) => any;
  onSelectWallet: (...args: Array<any>) => any;
  onBack: (...args: Array<any>) => any;
  wallets: Array<Wallet>;
  stepsList: Array<string>;
  minDelegationFunds: number;
  selectedWalletId: string | null | undefined;
  isWalletAcceptable: (...args: Array<any>) => any;
  getStakePoolById: (...args: Array<any>) => any;
};
type State = {
  selectedWalletId: string | null | undefined;
};
export default class DelegationStepsChooseWalletDialog extends Component<
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
      wallets,
      stepsList,
      minDelegationFunds,
      onClose,
      onBack,
      isWalletAcceptable,
      numberOfStakePools,
      getStakePoolById,
    } = this.props;
    const selectedWallet: Wallet | null | undefined = wallets.find(
      (wallet: Wallet) => wallet && wallet.id === selectedWalletId
    );
    const { amount, reward, isRestoring } = selectedWallet || {};
    let errorMessage;

    if (selectedWallet && !isWalletAcceptable(amount, reward)) {
      // Wallet is restoring
      if (isRestoring) errorMessage = messages.errorRestoringWallet;
      // Wallet only has Reward balance
      else if (!amount.isZero() && amount.isEqualTo(reward))
        errorMessage = messages.errorMinDelegationFundsRewardsOnly;
      // Wallet balance < min delegation funds
      else errorMessage = messages.errorMinDelegationFunds;
    }

    const error = errorMessage && (
      <p className={styles.errorMessage}>
        <FormattedHTMLMessage
          {...errorMessage}
          values={{
            minDelegationFunds,
          }}
        />
      </p>
    );
    const dialogClassName = classNames([
      commonStyles.delegationSteps,
      styles.delegationStepsChooseWalletDialogWrapper,
    ]);
    const contentClassName = classNames([commonStyles.content, styles.content]);
    const walletSelectClasses = classNames([
      styles.walletSelect,
      error ? styles.error : null,
    ]);
    const actions = [
      {
        className: 'continueButton',
        label: intl.formatMessage(messages.continueButtonLabel),
        onClick: this.onSelectWallet.bind(this),
        primary: true,
        disabled: !selectedWalletId || !!error,
      },
    ];
    const stepsIndicatorLabel = (
      <FormattedMessage
        {...messages.stepIndicatorLabel}
        values={{
          currentStep: 1,
          totalSteps: stepsList.length,
        }}
      />
    );
    return (
      <Dialog
        title={intl.formatMessage(messages.title)}
        subtitle={stepsIndicatorLabel}
        actions={actions}
        closeOnOverlayClick
        onClose={onClose}
        className={dialogClassName}
        closeButton={<DialogCloseButton onClose={onClose} />}
        backButton={<DialogBackButton onBack={onBack} />}
      >
        <div className={commonStyles.delegationStepsIndicatorWrapper}>
          <Stepper
            steps={stepsList}
            activeStep={1}
            skin={StepperSkin}
            labelDisabled
          />
        </div>

        <div className={contentClassName}>
          <p className={styles.description}>
            <FormattedHTMLMessage
              {...messages.description}
              values={{
                minDelegationFunds,
              }}
            />
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
        </div>
      </Dialog>
    );
  }
}
