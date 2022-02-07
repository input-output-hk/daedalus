import React, { Component } from 'react';
import { intlShape, FormattedMessage, FormattedHTMLMessage } from 'react-intl';
import vjf from 'mobx-react-form/lib/validators/VJF';
import classNames from 'classnames';
import { get } from 'lodash';
import { observer } from 'mobx-react';
import { Stepper } from 'react-polymorph/lib/components/Stepper';
import { StepperSkin } from 'react-polymorph/lib/skins/simple/StepperSkin';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './DelegationSteps.scss' or its... Remove this comment to see the full error message
import commonStyles from './DelegationSteps.scss';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './DelegationStepsConfirmationD... Remove this comment to see the full error message
import styles from './DelegationStepsConfirmationDialog.scss';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import DialogBackButton from '../../widgets/DialogBackButton';
import Dialog from '../../widgets/Dialog';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import { formattedWalletAmount } from '../../../utils/formatters';
import { submitOnEnter } from '../../../utils/form';
import globalMessages from '../../../i18n/global-messages';
import LocalizableError from '../../../i18n/LocalizableError';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';
import Wallet, { HwDeviceStatuses } from '../../../domains/Wallet';
import StakePool from '../../../domains/StakePool';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import HardwareWalletStatus from '../../hardware-wallet/HardwareWalletStatus';
import { getMessages } from './DelegationStepsConfirmationDialog.messages';
import { OversaturationText } from './OversaturationText';
import type { DelegationCalculateFeeResponse } from '../../../api/staking/types';
import type { HwDeviceStatus } from '../../../domains/Wallet';
import { ReactIntlMessage } from '../../../types/i18nTypes';

const messages: Record<string, ReactIntlMessage> = {
  ...getMessages(),
  fieldIsRequired: globalMessages.fieldIsRequired,
};
type Props = {
  onBack: (...args: Array<any>) => any;
  onClose: (...args: Array<any>) => any;
  onConfirm: (...args: Array<any>) => any;
  transactionFee: DelegationCalculateFeeResponse | null | undefined;
  selectedWallet: Wallet | null | undefined;
  selectedPool: StakePool | null | undefined;
  stepsList: Array<string>;
  isSubmitting: boolean;
  hwDeviceStatus: HwDeviceStatus;
  error: LocalizableError | null | undefined;
  onExternalLinkClick: (...args: Array<any>) => any;
  isTrezor: boolean;
  oversaturationPercentage: number;
};

@observer
class DelegationStepsConfirmationDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  form = new ReactToolboxMobxForm(
    // @ts-ignore ts-migrate(2554) FIXME: Expected 0 arguments, but got 2.
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
              const isHardwareWallet = get(
                this.props.selectedWallet,
                'isHardwareWallet'
              );
              if (isHardwareWallet) return [true];

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
      plugins: {
        vjf: vjf(),
      },
      options: {
        validateOnChange: true,
        validationDebounceWait: FORM_VALIDATION_DEBOUNCE_WAIT,
      },
    }
  );
  submit = () => {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'submit' does not exist on type 'ReactToo... Remove this comment to see the full error message
    this.form.submit({
      onSuccess: (form) => {
        const { selectedWallet } = this.props;
        const isHardwareWallet = get(selectedWallet, 'isHardwareWallet');
        const { spendingPassword } = form.values();
        this.props.onConfirm(spendingPassword, isHardwareWallet);
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
      hwDeviceStatus,
      onExternalLinkClick,
      isTrezor,
      oversaturationPercentage,
    } = this.props;
    const selectedWalletName = get(selectedWallet, 'name');
    const isHardwareWallet = get(selectedWallet, 'isHardwareWallet');
    const selectedPoolTicker = get(selectedPool, 'ticker');
    const selectedPoolId = get(selectedPool, 'id');
    // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
    const spendingPasswordField = form.$('spendingPassword');
    const buttonLabel = !isSubmitting ? (
      intl.formatMessage(messages.confirmButtonLabel)
    ) : (
      <LoadingSpinner />
    );
    const actions = [
      {
        className: 'cancelButton',
        label: intl.formatMessage(messages.cancelButtonLabel),
        onClick: !isSubmitting ? onClose : () => {},
        disabled: isSubmitting,
      },
      {
        className: 'confirmButton',
        label: buttonLabel,
        onClick: this.submit,
        primary: true,
        disabled:
          (!isHardwareWallet && !spendingPasswordField.isValid) ||
          (isHardwareWallet &&
            hwDeviceStatus !==
              HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED) ||
          isSubmitting ||
          !transactionFee,
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
    return (
      <Dialog
        title={intl.formatMessage(messages.title)}
        subtitle={stepsIndicatorLabel}
        actions={actions}
        closeOnOverlayClick
        onClose={!isSubmitting ? onClose : () => {}}
        className={dialogClassName}
        closeButton={<DialogCloseButton onClose={onClose} />}
        backButton={
          <DialogBackButton onBack={!isSubmitting ? onBack : () => {}} />
        }
      >
        <div className={commonStyles.delegationStepsIndicatorWrapper}>
          <Stepper
            steps={stepsList}
            activeStep={3}
            skin={StepperSkin}
            labelDisabled
          />
        </div>
        {oversaturationPercentage > 0 && (
          <OversaturationText
            oversaturationPercentage={oversaturationPercentage.toFixed(2)}
          />
        )}
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

          <div className={styles.stakePoolIdWrapper}>
            <p className={styles.stakePoolIdLabel}>
              {intl.formatMessage(messages.stakePoolIdLabel)}
            </p>
            <p className={styles.stakePoolId}>{selectedPoolId}</p>
          </div>

          <div className={styles.feesRow}>
            <div className={styles.feesWrapper}>
              <p className={styles.feesLabel}>
                {intl.formatMessage(messages.feesLabel)}
              </p>
              <p className={styles.feesAmount}>
                {!transactionFee ? (
                  <span className={styles.calculatingFeesLabel}>
                    {intl.formatMessage(messages.calculatingFees)}
                  </span>
                ) : (
                  <>
                    <span>
                      {formattedWalletAmount(transactionFee.fee, false)}
                    </span>
                    <span className={styles.feesAmountLabel}>
                      {` `}
                      {intl.formatMessage(globalMessages.adaUnit)}
                    </span>
                  </>
                )}
              </p>
            </div>
            {transactionFee &&
              transactionFee.deposits.isZero &&
              !transactionFee.deposits.isZero() && (
                <>
                  <div className={styles.depositWrapper}>
                    <p className={styles.depositLabel}>
                      {intl.formatMessage(messages.depositLabel)}
                    </p>
                    <p className={styles.depositAmount}>
                      <span>
                        {formattedWalletAmount(transactionFee.deposits, false)}
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
                walletName={selectedWalletName}
                isTrezor={isTrezor}
                onExternalLinkClick={onExternalLinkClick}
              />
            </div>
          ) : (
            <Input
              className={styles.spendingPassword}
              {...spendingPasswordField.bind()}
              skin={InputSkin}
              error={spendingPasswordField.error}
              onKeyPress={this.handleSubmitOnEnter}
            />
          )}
        </div>

        {error ? (
          <p className={styles.error}>{intl.formatMessage(error)}</p>
        ) : null}
      </Dialog>
    );
  }
}

export default DelegationStepsConfirmationDialog;
