// @flow
import React, { Component } from 'react';
import {
  defineMessages,
  intlShape,
  FormattedHTMLMessage,
  FormattedMessage,
} from 'react-intl';
import { get } from 'lodash';
import classNames from 'classnames';
import { Select } from 'react-polymorph/lib/components/Select';
import { SelectSkin } from 'react-polymorph/lib/skins/simple/SelectSkin';
import { Stepper } from 'react-polymorph/lib/components/Stepper';
import { StepperSkin } from 'react-polymorph/lib/skins/simple/StepperSkin';
import commonStyles from './DelegationSteps.scss';
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

type DelegationWalletData = {
  id: string,
  label: string,
  value: string,
  isAcceptableSetupWallet: boolean,
  hasPassword: boolean,
};

type Props = {
  onClose: Function,
  onSelectWallet: Function,
  onBack: Function,
  wallets: Array<DelegationWalletData>,
  stepsList: Array<string>,
  minDelegationFunds: number,
  selectedWallet: ?Object,
};

type State = {
  selectedWallet: ?DelegationWalletData,
};

export default class DelegationStepsChooseWalletDialog extends Component<
  Props,
  State
> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    selectedWallet: this.props.selectedWallet,
  };

  onWalletChange = (selectedWallet: DelegationWalletData) => {
    this.setState({ selectedWallet });
  };

  onSelectWallet = () => {
    const { selectedWallet } = this.state;
    const selectedWalletId = get(selectedWallet, 'id');
    this.props.onSelectWallet(selectedWalletId);
  };

  render() {
    const { intl } = this.context;
    const { selectedWallet } = this.state;
    const {
      wallets,
      stepsList,
      minDelegationFunds,
      onClose,
      onBack,
    } = this.props;

    const selectedWalletValue = get(selectedWallet, 'value', '');
    const isAcceptableSetupWallet = get(
      selectedWallet,
      'isAcceptableSetupWallet',
      false
    );

    const actions = [
      {
        className: 'continueButton',
        label: intl.formatMessage(messages.continueButtonLabel),
        onClick: this.onSelectWallet.bind(this),
        primary: true,
        disabled: !selectedWalletValue || !isAcceptableSetupWallet,
      },
    ];

    const dialogClassName = classNames([
      commonStyles.delegationSteps,
      styles.delegationStepsChooseWalletDialogWrapper,
    ]);
    const contentClassName = classNames([commonStyles.content, styles.content]);

    const walletSelectClasses = classNames([
      styles.walletSelect,
      selectedWallet && !isAcceptableSetupWallet ? styles.error : null,
    ]);

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
          <Select
            className={walletSelectClasses}
            label={intl.formatMessage(messages.selectWalletInputLabel)}
            options={wallets}
            optionRenderer={option => (
              <div
                className={styles.customOptionStyle}
                role="presentation"
                onClick={this.onWalletChange.bind(this, option)}
              >
                <div className={styles.optionLabel}>{option.label}</div>
                <div className={styles.optionValue}>{option.value}</div>
              </div>
            )}
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
            value={selectedWalletValue}
          />
          {selectedWallet && !isAcceptableSetupWallet && (
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
