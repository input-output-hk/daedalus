// @flow
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
import commonStyles from './DelegationSteps.scss';
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
      'MinDelegationFunds Error Label on the delegation setup "choose wallet" step dialog.',
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
  numberOfStakePools: number,
  onClose: Function,
  onSelectWallet: Function,
  onBack: Function,
  wallets: Array<Wallet>,
  stepsList: Array<string>,
  minDelegationFunds: number,
  selectedWalletId: ?string,
  isWalletAcceptable: Function,
  getStakePoolById: Function,
};

type State = {
  selectedWalletId: ?string,
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
      wallets,
      stepsList,
      minDelegationFunds,
      onClose,
      onBack,
      isWalletAcceptable,
      numberOfStakePools,
      getStakePoolById,
    } = this.props;

    const selectedWallet: ?Wallet =
      wallets.find(
        (wallet: Wallet) => wallet && wallet.id === selectedWalletId
      ) || null;
    const amount = selectedWallet ? selectedWallet.amount : null;
    const isAcceptableSetupWallet = amount && isWalletAcceptable(amount);

    const dialogClassName = classNames([
      commonStyles.delegationSteps,
      styles.delegationStepsChooseWalletDialogWrapper,
    ]);
    const contentClassName = classNames([commonStyles.content, styles.content]);

    const errorMinDelegationFunds = selectedWalletId &&
      !isAcceptableSetupWallet && (
        <p className={styles.errorMessage}>
          <FormattedHTMLMessage
            {...messages.errorMinDelegationFunds}
            values={{ minDelegationFunds }}
          />
        </p>
      );

    const errorRestoringWallet = selectedWallet &&
      selectedWallet.isRestoring && (
        <p className={styles.errorMessage}>
          {intl.formatMessage(messages.errorRestoringWallet)}
        </p>
      );

    const walletSelectClasses = classNames([
      styles.walletSelect,
      errorMinDelegationFunds || errorRestoringWallet ? styles.error : null,
    ]);

    const actions = [
      {
        className: 'continueButton',
        label: intl.formatMessage(messages.continueButtonLabel),
        onClick: this.onSelectWallet.bind(this),
        primary: true,
        disabled:
          !selectedWalletId ||
          !!errorMinDelegationFunds ||
          !!errorRestoringWallet,
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
              values={{ minDelegationFunds }}
            />
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
          {errorMinDelegationFunds}
          {errorRestoringWallet}
        </div>
      </Dialog>
    );
  }
}
