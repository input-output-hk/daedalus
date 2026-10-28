import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import vjf from 'mobx-react-form/lib/validators/VJF';
import classnames from 'classnames';
import type BigNumber from 'bignumber.js';
import { Input } from 'react-polymorph/lib/components/Input';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';
import { formattedWalletAmount } from '../../../utils/formatters';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import { HwDeviceStatuses } from '../../../domains/Wallet';
import HardwareWalletStatus from '../../hardware-wallet/HardwareWalletStatus';
import type { HwDeviceStatus } from '../../../domains/Wallet';
import type { UndelegationVoteStep } from '../../../stores/WalletsStore';
import { messages as governanceMessages } from '../../voting/voting-governance/VotingPowerDelegation.messages';
import { messages as apiErrorMessages } from '../../../api/errors';
import styles from './UndelegateWalletVoteFirstDialog.scss';
import globalMessages from '../../../i18n/global-messages';
import { messages as undelegateMessages } from './UndelegateWalletConfirmationDialog';
import { submitOnEnter } from '../../../utils/form';
import { withDiscreetMode } from '../../../features/discreet-mode';
import type { DiscreetModeFeature } from '../../../features/discreet-mode';
import { discreetWalletAmount } from '../../../features/discreet-mode/replacers/discreetWalletAmount';

const voteFirstMessages = defineMessages({
  description: {
    id: 'wallet.settings.undelegate.voteFirst.description',
    defaultMessage:
      "!!!<p>Undelegating <strong>{walletName}</strong> withdraws its {rewardsAmount} of staking rewards, and that withdrawal is refused while the wallet's voting power is not delegated, for example because its DRep retired.</p><p>Undelegation therefore takes two transactions. The first delegates the voting power to Abstain. Once it is confirmed, the second undelegates the wallet and withdraws the rewards.</p>",
    description:
      'Explanation at the start of a two-step undelegation; HTML as in descriptionWithTicker',
  },
  stepOneLabel: {
    id: 'wallet.settings.undelegate.voteFirst.stepOneLabel',
    defaultMessage: '!!!Step 1 of 2: delegate voting power to Abstain',
    description: 'Heading of the first step of a two-step undelegation',
  },
  confirmButtonLabel: {
    id: 'wallet.settings.undelegate.voteFirst.confirmButtonLabel',
    defaultMessage: '!!!Delegate to Abstain',
    description: 'Button that submits the first step',
  },
  waiting: {
    id: 'wallet.settings.undelegate.voteFirst.waiting',
    defaultMessage:
      '!!!Waiting for the delegation to Abstain to be confirmed. The undelegation follows once it is.',
    description:
      "Shown while the first step's transaction is not yet in the ledger",
  },
  voteNotConfirmed: {
    id: 'wallet.settings.undelegate.voteFirst.voteNotConfirmed',
    defaultMessage:
      '!!!The delegation to Abstain was not confirmed. No undelegation was sent.',
    description: "Shown when the first step's transaction failed or expired",
  },
});

const errorMessageFor = (
  errorCode: Extract<UndelegationVoteStep, { status: 'failed' }>['errorCode']
) => {
  switch (errorCode) {
    case 'vote_not_confirmed':
      return voteFirstMessages.voteNotConfirmed;
    case 'same_vote':
      return governanceMessages.transactionErrorSameVote;
    case 'pending_transaction':
      return apiErrorMessages.fundsHeldByPendingTransaction;
    case 'no_utxos_available':
    case 'not_enough_money':
      return governanceMessages.initializeNotEnoughMoney;
    case 'wrong_encryption_passphrase':
      return apiErrorMessages.wrongEncryptionPassphrase;
    default:
      return governanceMessages.initializeTxErrorGeneric;
  }
};

type Props = {
  walletId: string;
  walletName: string;
  heldReward: BigNumber;
  isHardwareWallet: boolean;
  // A wallet with no step yet reads as estimating.
  step: UndelegationVoteStep | null;
  hwDeviceStatus: HwDeviceStatus;
  isTrezor: boolean;
  onSubmit: (passphrase: string | undefined) => void;
  onRetry: () => void;
  onCancel: () => void;
  onClose: () => void;
  onExternalLinkClick: (url: string) => void;
};

type InjectedProps = {
  discreetModeFeature: DiscreetModeFeature;
};

interface FormFields {
  passphrase: string;
}

@observer
class UndelegateWalletVoteFirstDialog extends Component<Props & InjectedProps> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  form = new ReactToolboxMobxForm<FormFields>(
    {
      fields: {
        passphrase: {
          type: 'password',
          label: this.context.intl.formatMessage(
            undelegateMessages.spendingPasswordLabel
          ),
          placeholder: this.context.intl.formatMessage(
            undelegateMessages.spendingPasswordPlaceholder
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
    return this.props.step ? this.props.step.status : 'estimating';
  }

  get canCancel() {
    return this.status !== 'submitting' && this.status !== 'awaiting_ledger';
  }

  confirmationDisabled = () => {
    const { isHardwareWallet, hwDeviceStatus } = this.props;
    if (this.status !== 'ready') return true;
    if (isHardwareWallet) {
      return (
        hwDeviceStatus !== HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED
      );
    }
    return !this.form.$('passphrase').isValid;
  };

  handleSubmit = () => {
    if (this.confirmationDisabled()) return false;
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

  renderFee() {
    const { intl } = this.context;
    const { step } = this.props;
    const fee = step && step.status === 'ready' ? step.fee : null;
    const isEstimating = this.status === 'estimating';
    // The cip30 build prices nothing here: its approval dialog shows the fee.
    if (step && step.status === 'ready' && fee == null) return null;
    return (
      <div className={styles.feesRow}>
        <div className={styles.feesWrapper}>
          <p className={styles.feesLabel}>
            {intl.formatMessage(undelegateMessages.feesLabel)}
          </p>
          <p className={styles.feesAmount}>
            {isEstimating || fee == null ? (
              <span className={styles.calculatingFeesLabel}>
                {intl.formatMessage(undelegateMessages.calculatingFees)}
              </span>
            ) : (
              <>
                <span>{formattedWalletAmount(fee, false)}</span>
                <span className={styles.feesAmountLabel}>
                  {` `}
                  {intl.formatMessage(globalMessages.adaUnit)}
                </span>
              </>
            )}
          </p>
        </div>
      </div>
    );
  }

  render() {
    const { intl } = this.context;
    const {
      walletName,
      heldReward,
      isHardwareWallet,
      step,
      hwDeviceStatus,
      isTrezor,
      onRetry,
      onCancel,
      onClose,
      onExternalLinkClick,
      discreetModeFeature,
    } = this.props;
    const { status } = this;
    const passphraseField = this.form.$('passphrase');
    const isSubmitting = status === 'submitting';
    const onDismiss = this.canCancel ? onCancel : onClose;
    let actions;
    if (status === 'awaiting_ledger') {
      actions = [
        {
          label: intl.formatMessage(globalMessages.close),
          onClick: onClose,
        },
      ];
    } else if (status === 'failed') {
      actions = [
        {
          label: intl.formatMessage(globalMessages.cancel),
          onClick: onCancel,
        },
        {
          className: 'attention',
          label: intl.formatMessage(globalMessages.retry),
          onClick: onRetry,
          primary: true,
        },
      ];
    } else {
      actions = [
        ...(this.canCancel
          ? [
              {
                label: intl.formatMessage(globalMessages.cancel),
                onClick: onCancel,
              },
            ]
          : []),
        {
          className: classnames([
            'attention',
            isSubmitting ? styles.isSubmitting : null,
          ]),
          label: intl.formatMessage(voteFirstMessages.confirmButtonLabel),
          onClick: this.handleSubmit,
          disabled: this.confirmationDisabled(),
          primary: true,
        },
      ];
    }
    const showForm =
      status === 'estimating' || status === 'ready' || isSubmitting;
    return (
      <Dialog
        title={intl.formatMessage(undelegateMessages.title)}
        subtitle={walletName}
        actions={actions}
        closeOnOverlayClick
        onClose={onDismiss}
        className={styles.dialog}
        closeButton={<DialogCloseButton onClose={onDismiss} />}
      >
        <div className={styles.description}>
          <FormattedHTMLMessage
            {...voteFirstMessages.description}
            values={{
              walletName,
              rewardsAmount: discreetModeFeature.discreetValue({
                replacer: discreetWalletAmount({ amount: heldReward }),
              }),
            }}
          />
        </div>
        <p className={styles.stepLabel}>
          {intl.formatMessage(voteFirstMessages.stepOneLabel)}
        </p>
        {showForm && (
          <>
            <div className={styles.divider} />
            {this.renderFee()}
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
                disabled={isSubmitting}
                onKeyPress={this.handleSubmitOnEnter}
              />
            )}
          </>
        )}
        {status === 'awaiting_ledger' && (
          <p className={styles.waiting}>
            {intl.formatMessage(voteFirstMessages.waiting)}
          </p>
        )}
        {step && step.status === 'failed' && (
          <p className={styles.error}>
            <FormattedHTMLMessage
              {...errorMessageFor(step.errorCode)}
              values={{ walletId: this.props.walletId }}
            />
          </p>
        )}
      </Dialog>
    );
  }
}

export default withDiscreetMode<Props>(UndelegateWalletVoteFirstDialog);
