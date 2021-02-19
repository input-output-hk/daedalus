// @flow
/* eslint-disable jsx-a11y/label-has-associated-control, jsx-a11y/label-has-for */
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { get } from 'lodash';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import vjf from 'mobx-react-form/lib/validators/VJF';
import classnames from 'classnames';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { Input } from 'react-polymorph/lib/components/Input';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';
import { formattedWalletAmount } from '../../../utils/formatters';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import { FormattedHTMLMessageWithLink } from '../../widgets/FormattedHTMLMessageWithLink';
import Dialog from '../../widgets/Dialog';
import Wallet, { HwDeviceStatuses } from '../../../domains/Wallet';
import HardwareWalletStatus from '../../hardware-wallet/HardwareWalletStatus';
import type { DelegationCalculateFeeResponse } from '../../../api/staking/types';
import type { HwDeviceStatus } from '../../../domains/Wallet';
import styles from './UndelegateWalletConfirmationDialog.scss';
import globalMessages from '../../../i18n/global-messages';
import LocalizableError from '../../../i18n/LocalizableError';
import { submitOnEnter } from '../../../utils/form';

const messages = defineMessages({
  title: {
    id: 'wallet.settings.undelegate.dialog.title',
    defaultMessage: '!!!Undelegate',
    description: 'Title for the "Undelegate wallet" dialog.',
  },
  confirmButtonLabel: {
    id: 'wallet.settings.undelegate.dialog.confirmButtonLabel',
    defaultMessage: '!!!Undelegate',
    description:
      'Label for the "Undelegate" button in the undelegate wallet dialog.',
  },
  descriptionWithTicker: {
    id: 'wallet.settings.undelegate.dialog.descriptionWithTicker',
    defaultMessage:
      '!!!<p>The stake from your wallet <strong>{walletName}</strong> is currently delegated to the <strong>[{stakePoolTicker}] {stakePoolName}</strong> stake pool.</p><p>Do you want to undelegate your stake and stop earning rewards?</p>',
    description:
      'Description of current delegation of wallet in the "Undelegate wallet" dialog.',
  },
  descriptionWithUnknownTicker: {
    id: 'wallet.settings.undelegate.dialog.descriptionWithUnknownTicker',
    defaultMessage:
      '!!!<p>The stake from your wallet <strong>{walletName}</strong> is currently delegated to the <strong>{stakePoolTicker}</strong> stake pool.</p><p>Do you want to undelegate your stake and stop earning rewards?</p>',
    description:
      'Description of current delegation of wallet in the "Undelegate wallet" dialog.',
  },
  unknownStakePoolLabel: {
    id: 'wallet.settings.undelegate.dialog.unknownStakePoolLabel',
    defaultMessage: '!!!unknown',
    description: 'unknown stake pool label in the "Undelegate wallet" dialog.',
  },
  confirmUnsupportNotice: {
    id: 'wallet.settings.undelegate.dialog.confirmUnsupportNotice',
    defaultMessage:
      '!!!I understand that I am not supporting the Cardano network when my stake is undelegated.',
    description:
      'Notice to confirm if the user understands unsupporting Cardano network after undelegation',
  },
  confirmIneligibleNotice: {
    id: 'wallet.settings.undelegate.dialog.confirmIneligibleNotice',
    defaultMessage:
      '!!!I understand that I will not be eligible to earn rewards when my stake is undelegated.',
    description:
      'Notice to confirm if the user understands non-earning rewards after undelegation',
  },
  feesLabel: {
    id: 'wallet.settings.undelegate.dialog.feesLabel',
    defaultMessage: '!!!Fees',
    description: 'Fees label in the "Undelegate wallet" dialog.',
  },
  depositLabel: {
    id: 'wallet.settings.undelegate.dialog.depositLabel',
    defaultMessage: '!!!Deposits reclaimed',
    description: 'Deposits reclaimed label in the "Undelegate wallet" dialog.',
  },
  spendingPasswordLabel: {
    id: 'wallet.settings.undelegate.dialog.spendingPasswordLabel',
    defaultMessage: '!!!Spending password',
    description: 'Spending password label in the "Undelegate wallet" dialog.',
  },
  spendingPasswordPlaceholder: {
    id: 'wallet.settings.undelegate.dialog.spendingPasswordPlaceholder',
    defaultMessage: '!!!Type your spending password here',
    description:
      'Spending password placeholder in the "Undelegate wallet" dialog.',
  },
  passwordErrorMessage: {
    id: 'wallet.settings.undelegate.dialog.passwordError',
    defaultMessage: '!!!Incorrect spending password.',
    description: 'Label for password error in the "Undelegate wallet" dialog.',
  },
  calculatingFees: {
    id: 'wallet.settings.undelegate.dialog.calculatingFees',
    defaultMessage: '!!!Calculating fees',
    description:
      '"Calculating fees" message in the "Undelegate wallet" dialog.',
  },
});

messages.fieldIsRequired = globalMessages.fieldIsRequired;

type Props = {
  selectedWallet: ?Wallet,
  stakePoolName: ?string,
  stakePoolTicker: ?string,
  onConfirm: Function,
  onCancel: Function,
  onExternalLinkClick: Function,
  isSubmitting: boolean,
  error: ?LocalizableError,
  fees: ?DelegationCalculateFeeResponse,
  hwDeviceStatus: HwDeviceStatus,
};

@observer
export default class UndelegateWalletConfirmationDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  form = new ReactToolboxMobxForm(
    {
      fields: {
        confirmUnsupportChecked: {
          type: 'checkbox',
          label: this.context.intl.formatMessage(
            messages.confirmUnsupportNotice
          ),
          value: false,
          validators: [
            ({ field }) => {
              if (field.value === false) {
                return [
                  false,
                  this.context.intl.formatMessage(messages.fieldIsRequired),
                ];
              }
              return [true];
            },
          ],
        },
        confirmIneligibleChecked: {
          type: 'checkbox',
          label: this.context.intl.formatMessage(
            messages.confirmIneligibleNotice
          ),
          value: false,
          validators: [
            ({ field }) => {
              if (field.value === false) {
                return [
                  false,
                  this.context.intl.formatMessage(messages.fieldIsRequired),
                ];
              }
              return [true];
            },
          ],
        },
        passphrase: {
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
              const isHardwareWallet = get(
                this.props.selectedWallet,
                'isHardwareWallet'
              );
              if (isHardwareWallet) return [true];
              if (field.value === '') {
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
      plugins: { vjf: vjf() },
      options: {
        validateOnChange: true,
        validationDebounceWait: FORM_VALIDATION_DEBOUNCE_WAIT,
      },
    }
  );

  confirmationDisabled = () => {
    const { form } = this;
    const { fees, isSubmitting, hwDeviceStatus, selectedWallet } = this.props;
    const { isValid: unsupportCheckboxIsValid } = form.$(
      'confirmUnsupportChecked'
    );
    const { isValid: ineligibleCheckboxIsValid } = form.$(
      'confirmIneligibleChecked'
    );
    const { isValid: passphraseIsValid } = form.$('passphrase');
    const isHardwareWallet = get(selectedWallet, 'isHardwareWallet');

    if (isHardwareWallet) {
      return (
        hwDeviceStatus !== HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED
      );
    }

    return (
      isSubmitting ||
      !fees ||
      !unsupportCheckboxIsValid ||
      !ineligibleCheckboxIsValid ||
      !passphraseIsValid
    );
  };

  handleSubmit = () => {
    if (this.confirmationDisabled()) {
      return false;
    }

    return this.form.submit({
      onSuccess: (form) => {
        const { selectedWallet, onConfirm } = this.props;
        const isHardwareWallet = get(selectedWallet, 'isHardwareWallet');
        const { passphrase } = form.values();
        onConfirm(passphrase, isHardwareWallet);
      },
      onError: () => null,
    });
  };

  handleSubmitOnEnter = (event: KeyboardEvent) =>
    submitOnEnter(this.handleSubmit, event);

  generateErrorElement = () => {
    const { error, onExternalLinkClick } = this.props;

    if (!error) {
      return null;
    }

    const errorHasLink = !!get(error, 'values.linkLabel', false);
    const result = errorHasLink ? (
      <FormattedHTMLMessageWithLink
        message={error}
        onExternalLinkClick={onExternalLinkClick}
      />
    ) : (
      this.context.intl.formatMessage(error)
    );

    return result;
  };

  render() {
    const { form } = this;
    const { intl } = this.context;
    const unsupportCheckboxField = form.$('confirmUnsupportChecked');
    const ineligibleCheckboxField = form.$('confirmIneligibleChecked');
    const passphraseField = form.$('passphrase');
    const {
      selectedWallet,
      stakePoolName,
      stakePoolTicker,
      onCancel,
      isSubmitting,
      fees,
      hwDeviceStatus,
      onExternalLinkClick,
    } = this.props;
    const walletName = get(selectedWallet, 'name');
    const isHardwareWallet = get(selectedWallet, 'isHardwareWallet');
    const confirmationDisabled = this.confirmationDisabled();
    const buttonClasses = classnames([
      'attention',
      isSubmitting ? styles.isSubmitting : null,
    ]);
    const actions = [
      {
        label: intl.formatMessage(globalMessages.cancel),
        onClick: !isSubmitting ? onCancel : () => null,
      },
      {
        className: buttonClasses,
        label: intl.formatMessage(messages.confirmButtonLabel),
        onClick: this.handleSubmit,
        disabled: confirmationDisabled,
        primary: true,
      },
    ];
    const errorElement = this.generateErrorElement();

    return (
      <Dialog
        title={intl.formatMessage(messages.title)}
        actions={actions}
        closeOnOverlayClick
        onClose={!isSubmitting ? onCancel : () => null}
        className={styles.dialog}
        closeButton={
          <DialogCloseButton onClose={!isSubmitting ? onCancel : () => null} />
        }
      >
        <div className={styles.description}>
          {stakePoolTicker ? (
            <FormattedHTMLMessage
              {...messages.descriptionWithTicker}
              values={{ walletName, stakePoolName, stakePoolTicker }}
            />
          ) : (
            <FormattedHTMLMessage
              {...messages.descriptionWithUnknownTicker}
              values={{
                walletName,
                stakePoolTicker: intl.formatMessage(
                  messages.unknownStakePoolLabel
                ),
              }}
            />
          )}
        </div>
        <Checkbox
          {...unsupportCheckboxField.bind()}
          error={unsupportCheckboxField.error}
        />
        <Checkbox
          {...ineligibleCheckboxField.bind()}
          error={ineligibleCheckboxField.error}
        />
        <div className={styles.divider} />
        <div className={styles.feesRow}>
          <div className={styles.feesWrapper}>
            <p className={styles.feesLabel}>
              {intl.formatMessage(messages.feesLabel)}
            </p>
            <p className={styles.feesAmount}>
              {!fees || !fees.fee ? (
                <span className={styles.calculatingFeesLabel}>
                  {intl.formatMessage(messages.calculatingFees)}
                </span>
              ) : (
                <>
                  <span>{formattedWalletAmount(fees.fee, false)}</span>
                  <span className={styles.feesAmountLabel}>
                    {` `}
                    {intl.formatMessage(globalMessages.unitAda)}
                  </span>
                </>
              )}
            </p>
          </div>
          {fees && !fees.depositsReclaimed.isZero() && (
            <>
              <div className={styles.depositWrapper}>
                <p className={styles.depositLabel}>
                  {intl.formatMessage(messages.depositLabel)}
                </p>
                <p className={styles.depositAmount}>
                  <span>
                    {formattedWalletAmount(fees.depositsReclaimed, false)}
                  </span>
                  <span className={styles.depositAmountLabel}>
                    {` `}
                    {intl.formatMessage(globalMessages.unitAda)}
                  </span>
                </p>
              </div>
            </>
          )}
        </div>
        {isHardwareWallet ? (
          <div className={styles.hardwareWalletStatusWrapper}>
            <HardwareWalletStatus
              hwDeviceStatus={hwDeviceStatus}
              walletName={walletName}
              onExternalLinkClick={onExternalLinkClick}
            />
          </div>
        ) : (
          <Input
            type="password"
            {...passphraseField.bind()}
            error={passphraseField.error}
            onKeyPress={this.handleSubmitOnEnter}
          />
        )}
        {errorElement && <p className={styles.error}>{errorElement}</p>}
      </Dialog>
    );
  }
}
