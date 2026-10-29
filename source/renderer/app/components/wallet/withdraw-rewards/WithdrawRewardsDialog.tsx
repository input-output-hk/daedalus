import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { intlShape, FormattedHTMLMessage } from 'react-intl';
import vjf from 'mobx-react-form/lib/validators/VJF';
import classnames from 'classnames';
import { Input } from 'react-polymorph/lib/components/Input';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';
import { formattedWalletAmount } from '../../../utils/formatters';
import { submitOnEnter } from '../../../utils/form';
import Dialog from '../../widgets/Dialog';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import HardwareWalletStatus from '../../hardware-wallet/HardwareWalletStatus';
import { HwDeviceStatuses } from '../../../domains/Wallet';
import type { HwDeviceStatus } from '../../../domains/Wallet';
import type { RewardWithdrawalStep } from '../../../stores/WalletsStore';
import globalMessages from '../../../i18n/global-messages';
import { messages as apiErrorMessages } from '../../../api/errors';
import { messages as sendMessages } from '../../../containers/wallet/dialogs/send-confirmation/messages';
import sendFormMessages from '../send-form/messages';
import { withDiscreetMode } from '../../../features/discreet-mode';
import type { DiscreetModeFeature } from '../../../features/discreet-mode';
import { discreetWalletAmount } from '../../../features/discreet-mode/replacers/discreetWalletAmount';
import { messages } from './WithdrawRewardsDialog.messages';
import styles from './WithdrawRewardsDialog.scss';

type Props = {
  walletName: string;
  isHardwareWallet: boolean;
  // A wallet with no step yet reads as preparing.
  step: RewardWithdrawalStep | null;
  // A hardware wallet's submission runs in the hardware wallets store.
  isSubmitting?: boolean;
  hwDeviceStatus: HwDeviceStatus;
  isTrezor: boolean;
  onSubmit: (passphrase: string | undefined) => void;
  onCancel: () => void;
  onExternalLinkClick: (url: string) => void;
};

type InjectedProps = {
  discreetModeFeature: DiscreetModeFeature;
};

interface FormFields {
  passphrase: string;
}

@observer
class WithdrawRewardsDialog extends Component<Props & InjectedProps> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  form = new ReactToolboxMobxForm<FormFields>(
    {
      fields: {
        passphrase: {
          type: 'password',
          label: this.context.intl.formatMessage(sendMessages.passphraseLabel),
          placeholder: this.context.intl.formatMessage(
            sendMessages.passphraseFieldPlaceholder
          ),
          value: '',
          validators: [
            ({ field }) => {
              if (this.props.isHardwareWallet) return [true];
              if (field.value === '') {
                return [
                  false,
                  this.context.intl.formatMessage(
                    globalMessages.fieldIsRequired
                  ),
                ];
              }
              return [true];
            },
          ],
        },
      },
    },
    {
      plugins: {
        vjf: vjf(),
      },
      options: {
        validateOnChange: true,
        validationDebounceWait: FORM_VALIDATION_DEBOUNCE_WAIT,
      },
    }
  );

  get status() {
    return this.props.step ? this.props.step.status : 'preparing';
  }

  submitDisabled = () => {
    const { isHardwareWallet, hwDeviceStatus } = this.props;
    if (this.status !== 'ready' || this.props.isSubmitting) return true;
    if (isHardwareWallet) {
      return (
        hwDeviceStatus !== HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED
      );
    }
    return !this.form.$('passphrase').isValid;
  };

  handleSubmit = () => {
    if (this.submitDisabled()) return false;
    return this.form.submit({
      onSuccess: (form) => {
        const { passphrase } = form.values();
        this.props.onSubmit(
          this.props.isHardwareWallet ? undefined : passphrase
        );
      },
      onError: () => null,
    });
  };

  handleSubmitOnEnter = (event: KeyboardEvent) =>
    submitOnEnter(this.handleSubmit, event);

  render() {
    const { intl } = this.context;
    const {
      walletName,
      isHardwareWallet,
      step,
      hwDeviceStatus,
      isTrezor,
      onCancel,
      onExternalLinkClick,
      discreetModeFeature,
    } = this.props;
    const { status } = this;
    const passphraseField = this.form.$('passphrase');
    const isSubmitting = status === 'submitting' || !!this.props.isSubmitting;
    const actions = [
      ...(isSubmitting
        ? []
        : [
            {
              label: intl.formatMessage(globalMessages.cancel),
              onClick: onCancel,
            },
          ]),
      ...(status === 'failed'
        ? []
        : [
            {
              className: classnames([
                'attention',
                isSubmitting ? styles.isSubmitting : null,
              ]),
              label: intl.formatMessage(messages.submit),
              onClick: this.handleSubmit,
              disabled: this.submitDisabled(),
              primary: true,
            },
          ]),
    ];
    const ready = step && (step.status === 'ready' ? step : null);
    const showForm = status === 'ready' || isSubmitting;
    const stepError =
      step && (step.status === 'failed' || step.status === 'ready')
        ? step.error
        : null;
    // A message needs an id to be drawn; anything else is an internal error.
    const error =
      stepError && typeof stepError.id !== 'string'
        ? apiErrorMessages.walletInternalError
        : stepError;
    return (
      <Dialog
        title={intl.formatMessage(messages.title)}
        subtitle={walletName}
        actions={actions}
        closeOnOverlayClick={!isSubmitting}
        onClose={isSubmitting ? undefined : onCancel}
        className={styles.dialog}
        closeButton={
          isSubmitting ? undefined : <DialogCloseButton onClose={onCancel} />
        }
      >
        <p className={styles.description}>
          {intl.formatMessage(messages.description)}
        </p>
        {status === 'preparing' && (
          <p className={styles.calculating}>
            {intl.formatMessage(sendFormMessages.calculatingFeesLabel)}
          </p>
        )}
        {ready && (
          <div className={styles.amounts}>
            <div className={styles.amountRow}>
              <p className={styles.label}>
                {intl.formatMessage(messages.amountLabel)}
              </p>
              <p className={styles.amount}>
                {discreetModeFeature.discreetValue({
                  replacer: discreetWalletAmount({ amount: ready.amount }),
                })}
              </p>
            </div>
            <div className={styles.amountRow}>
              <p className={styles.label}>
                {intl.formatMessage(sendMessages.feesLabel)}
              </p>
              <p className={styles.amount}>
                {formattedWalletAmount(ready.fee, false)}{' '}
                {intl.formatMessage(globalMessages.adaUnit)}
              </p>
            </div>
          </div>
        )}
        {showForm &&
          (isHardwareWallet ? (
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
              disabled={isSubmitting}
              onKeyPress={this.handleSubmitOnEnter}
              autoFocus
            />
          ))}
        {error && (
          <p className={styles.error} role="alert">
            <FormattedHTMLMessage {...error} />
          </p>
        )}
      </Dialog>
    );
  }
}

export default withDiscreetMode<Props>(WithdrawRewardsDialog);
