// @flow
/* eslint-disable jsx-a11y/label-has-associated-control, jsx-a11y/label-has-for */
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { get } from 'lodash';
import { intlShape, FormattedHTMLMessage } from 'react-intl';
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
import getUndelegateWalletConfirmationDialogMessages from './UndelegateWalletConfirmationDialog.messages';

const messages = getUndelegateWalletConfirmationDialogMessages();

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
  isTrezor: boolean,
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
      isTrezor,
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
        subtitle={walletName}
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
                    {intl.formatMessage(globalMessages.adaUnit)}
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
                    {intl.formatMessage(globalMessages.adaUnit)}
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
              isTrezor={isTrezor}
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
