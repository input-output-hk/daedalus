// @flow
import React, { Component } from 'react';
import {
  defineMessages,
  intlShape,
  FormattedMessage,
  FormattedHTMLMessage,
} from 'react-intl';
import classNames from 'classnames';
import { get } from 'lodash';
import { observer } from 'mobx-react';
import { Stepper } from 'react-polymorph/lib/components/Stepper';
import { StepperSkin } from 'react-polymorph/lib/skins/simple/StepperSkin';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import BigNumber from 'bignumber.js';
import commonStyles from './DelegationSteps.scss';
import styles from './DelegationStepsConfirmationDialog.scss';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import DialogBackButton from '../../widgets/DialogBackButton';
import Dialog from '../../widgets/Dialog';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import { submitOnEnter } from '../../../utils/form';
import globalMessages from '../../../i18n/global-messages';
import LocalizableError from '../../../i18n/LocalizableError';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';
import { DECIMAL_PLACES_IN_ADA } from '../../../config/numbersConfig';
import Wallet from '../../../domains/Wallet';
import type { StakePool } from '../../../api/staking/types';

const messages = defineMessages({
  title: {
    id: 'staking.delegationSetup.confirmation.step.dialog.title',
    defaultMessage: '!!!Confirm Delegation',
    description:
      'Title "Confirm Delegation" on the delegation setup "confirmation" step dialog.',
  },
  stepIndicatorLabel: {
    id: 'staking.delegationSetup.confirmation.step.dialog.stepIndicatorLabel',
    defaultMessage: '!!!STEP {currentStep} OF {totalSteps}',
    description:
      'Step indicator labe on the delegation setup "confirmation" step dialog.',
  },
  description: {
    id: 'staking.delegationSetup.confirmation.step.dialog.description',
    defaultMessage:
      '!!!Confirm your delegation for <span>{selectedWalletName}<span> wallet to <span>[{selectedPoolTicker}]<span> stake pool by posting your delegation preferences on the Cardano blockchain.',
    description:
      'Description on the delegation setup "confirmation" step dialog.',
  },
  feesLabel: {
    id: 'staking.delegationSetup.confirmation.step.dialog.feesLabel',
    defaultMessage: '!!!Fees',
    description:
      'Fees label on the delegation setup "confirmation" step dialog.',
  },
  spendingPasswordPlaceholder: {
    id:
      'staking.delegationSetup.confirmation.step.dialog.spendingPasswordPlaceholder',
    defaultMessage: '!!!Spending password',
    description: 'Placeholder for "spending password"',
  },
  spendingPasswordLabel: {
    id:
      'staking.delegationSetup.confirmation.step.dialog.spendingPasswordLabel',
    defaultMessage: '!!!Spending password',
    description: 'Label for "spending password"',
  },
  confirmButtonLabel: {
    id: 'staking.delegationSetup.confirmation.step.dialog.confirmButtonLabel',
    defaultMessage: '!!!Confirm',
    description:
      'Label for continue button on the delegation setup "confirmation" step dialog.',
  },
  cancelButtonLabel: {
    id: 'staking.delegationSetup.confirmation.step.dialog.cancelButtonLabel',
    defaultMessage: '!!!Cancel',
    description:
      'Label for "Cancel" button on the delegation setup "confirmation" step dialog.',
  },
});

messages.fieldIsRequired = globalMessages.fieldIsRequired;

type Props = {
  onBack: Function,
  onClose: Function,
  onConfirm: Function,
  transactionFee: BigNumber,
  selectedWallet: Wallet,
  selectedPool: StakePool,
  stepsList: Array<string>,
  isSubmitting: boolean,
  error: ?LocalizableError,
};

@observer
export default class DelegationStepsConfirmationDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  form = new ReactToolboxMobxForm(
    {
      fields: {
        spendingPassword: {
          type: 'password',
          label: this.context.intl.formatMessage(
            messages.spendingPasswordLabel
          ),
          placeholder: this.context.intl.formatMessage(
            messages.spendingPasswordPlaceholder
          ),
          value: '',
          validators: [
            ({ field }) => {
              const password = field.value;
              if (password === '') {
                return [
                  false,
                  this.context.intl.formatMessage(messages.fieldIsRequired),
                ];
              }
              return [true];
            },
          ],
        },
      },
    },
    {
      options: {
        validateOnChange: true,
        validationDebounceWait: FORM_VALIDATION_DEBOUNCE_WAIT,
      },
    }
  );

  submit = () => {
    this.form.submit({
      onSuccess: form => {
        const { spendingPassword } = form.values();
        this.props.onConfirm(spendingPassword);
      },
      onError: () => {},
    });
  };

  handleSubmitOnEnter = submitOnEnter.bind(this, this.submit);

  render() {
    const { form } = this;
    const { intl } = this.context;
    const {
      onBack,
      onClose,
      stepsList,
      transactionFee,
      selectedPool,
      selectedWallet,
      error,
      isSubmitting,
    } = this.props;
    const selectedWalletName = get(selectedWallet, 'name');
    const selectedPoolTicker = get(selectedPool, 'slug');
    const spendingPasswordField = form.$('spendingPassword');

    const confirmButtonClasses = classNames([
      'confirmButton',
      isSubmitting ? styles.submitButtonSpinning : null,
    ]);

    const actions = [
      {
        className: 'cancelButton',
        label: intl.formatMessage(messages.cancelButtonLabel),
        onClick: !isSubmitting ? onClose : () => {},
      },
      {
        className: confirmButtonClasses,
        label: intl.formatMessage(messages.confirmButtonLabel),
        onClick: this.submit,
        primary: true,
        disabled: !spendingPasswordField.isValid,
      },
    ];

    const dialogClassName = classNames([
      commonStyles.delegationSteps,
      styles.delegationStepsConfirmationDialogWrapper,
    ]);
    const contentClassName = classNames([commonStyles.content, styles.content]);

    const stepsIndicatorLabel = (
      <FormattedMessage
        {...messages.stepIndicatorLabel}
        values={{
          currentStep: 3,
          totalSteps: stepsList.length,
        }}
      />
    );

    const fees = transactionFee.toFormat(DECIMAL_PLACES_IN_ADA);

    return (
      <Dialog
        title={intl.formatMessage(messages.title)}
        subtitle={stepsIndicatorLabel}
        actions={actions}
        closeOnOverlayClick
        onClose={!isSubmitting ? onClose : () => {}}
        className={dialogClassName}
        closeButton={<DialogCloseButton onClose={onClose} />}
        backButton={<DialogBackButton onBack={onBack} />}
      >
        <div className={commonStyles.delegationStepsIndicatorWrapper}>
          <Stepper
            steps={stepsList}
            activeStep={3}
            skin={StepperSkin}
            labelDisabled
          />
        </div>

        <div className={contentClassName}>
          <p className={styles.description}>
            <FormattedHTMLMessage
              {...messages.description}
              values={{
                selectedWalletName,
                selectedPoolTicker,
              }}
            />
          </p>

          <div className={styles.feesWrapper}>
            <p className={styles.feesLabel}>
              {intl.formatMessage(messages.feesLabel)}
            </p>
            <p className={styles.feesAmount}>
              {fees}
              <span> ADA</span>
            </p>
          </div>

          <Input
            className={styles.spendingPassword}
            {...spendingPasswordField.bind()}
            skin={InputSkin}
            error={spendingPasswordField.error}
            onKeyPress={this.handleSubmitOnEnter}
          />
        </div>

        {error ? (
          <p className={styles.error}>{intl.formatMessage(error)}</p>
        ) : null}
      </Dialog>
    );
  }
}
