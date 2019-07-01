// @flow
import React, { Component } from 'react';
import {
  defineMessages,
  intlShape,
  FormattedHTMLMessage,
  FormattedMessage,
} from 'react-intl';
import classNames from 'classnames';
import { Select } from 'react-polymorph/lib/components/Select';
import { SelectSkin } from 'react-polymorph/lib/skins/simple/SelectSkin';
import { Stepper } from 'react-polymorph/lib/components/Stepper';
import { StepperSkin } from 'react-polymorph/lib/skins/simple/StepperSkin';
import styles from './DelegationStepsChooseWalletDialog.scss';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import DialogBackButton from '../../widgets/DialogBackButton';
import Dialog from '../../widgets/Dialog';

const messages = defineMessages({
  title: {
    id: 'staking.delegationSetup.chooseWallet.step.dialog.title',
    defaultMessage: '!!!Delegation Setup',
    description:
      'Title "Delegation Setup" on the delegation setup "choose wallet" step dialog.',
  },
  description: {
    id: 'staking.delegationSetup.chooseWallet.step.dialog.description',
    defaultMessage:
      '!!!Choose a wallet with funds you would like to delegate to a stake pool. Selected wallet needs to have a minimum of <span>{minDelegationFunds} ada</span>.',
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
      'Step indicator labe on the delegation setup "choose wallet" step dialog.',
  },
  errorMessage: {
    id: 'staking.delegationSetup.chooseWallet.step.dialog.errorMessage',
    defaultMessage:
      '!!!This wallet does not have enough ada for delegation setup. Please choose a wallet with a minimum of <span>{minDelegationFunds} ada</span> and click continue.',
    description:
      'Error Label on the delegation setup "choose wallet" step dialog.',
  },
  continueButtonLabel: {
    id: 'staking.delegationSetup.chooseWallet.step.dialog.continueButtonLabel',
    defaultMessage: '!!!Continue',
    description:
      'Label for continue button on the delegation setup "choose wallet" step dialog.',
  },
});

type WalletData = {
  label: string,
  value: string,
  isAcceptableSetupWallet: boolean,
};

type Props = {
  onClose: Function,
  onContinue: Function,
  onBack: Function,
  wallets: Array<WalletData>,
  stepsList: Array<string>,
  minDelegationFunds: number,
};

type State = {
  selectedWalletAmount: string,
  walletChoiceError: boolean,
};

export default class DelegationStepsChooseWalletDialog extends Component<
  Props,
  State
> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    selectedWalletAmount: '',
    walletChoiceError: false,
  };

  onWalletChange = (selectedWallet: WalletData) => {
    this.setState({
      walletChoiceError: !selectedWallet.isAcceptableSetupWallet,
      selectedWalletAmount: selectedWallet.value,
    });
  };

  render() {
    const { intl } = this.context;
    const { walletChoiceError, selectedWalletAmount } = this.state;
    const {
      wallets,
      stepsList,
      minDelegationFunds,
      onClose,
      onContinue,
      onBack,
    } = this.props;

    const actions = [
      {
        className: 'continueButton',
        label: intl.formatMessage(messages.continueButtonLabel),
        onClick: onContinue,
        primary: true,
        disabled: !selectedWalletAmount || walletChoiceError,
      },
    ];

    const walletSelectClasses = classNames([
      styles.walletSelect,
      walletChoiceError ? styles.error : null,
    ]);

    return (
      <Dialog
        title={intl.formatMessage(messages.title)}
        actions={actions}
        closeOnOverlayClick
        onClose={onClose}
        className={styles.delegationStepsChooseWalletDialogWrapper}
        closeButton={<DialogCloseButton onClose={onClose} />}
        backButton={<DialogBackButton onBack={onBack} />}
      >
        <div className={styles.delegationStepsIndicatorWrapper}>
          <p className={styles.stepIndicatorLabel}>
            <FormattedMessage
              {...messages.stepIndicatorLabel}
              values={{
                currentStep: 1,
                totalSteps: stepsList.length,
              }}
            />
          </p>
          <Stepper
            steps={stepsList}
            activeStep={1}
            skin={StepperSkin}
            labelDisabled
          />
        </div>

        <div className={styles.content}>
          <p className={styles.description}>
            <FormattedHTMLMessage
              {...messages.description}
              values={{ minDelegationFunds }}
            />
          </p>
          <Select
            className={walletSelectClasses}
            label={intl.formatMessage(messages.selectWalletInputLabel)}
            options={wallets}
            optionRenderer={option => {
              return (
                <div
                  className={styles.customOptionStyle}
                  role="presentation"
                  onClick={this.onWalletChange.bind(this, option)}
                >
                  <div className={styles.optionLabel}>{option.label}</div>
                  <div className={styles.optionValue}>{option.value}</div>
                </div>
              );
            }}
            selectionRenderer={option => (
              <div className={styles.customValueStyle}>
                <div className={styles.label}>{option.label}</div>
                <div className={styles.value}>{option.value}</div>
              </div>
            )}
            placeholder={intl.formatMessage(
              messages.selectWalletInputPlaceholder
            )}
            skin={SelectSkin}
            value={selectedWalletAmount}
          />
          {walletChoiceError && (
            <p className={styles.errorMessage}>
              <FormattedHTMLMessage
                {...messages.errorMessage}
                values={{ minDelegationFunds }}
              />
            </p>
          )}
        </div>
      </Dialog>
    );
  }
}
