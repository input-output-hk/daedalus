// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
// import classnames from 'classnames';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import vjf from 'mobx-react-form/lib/validators/VJF';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import styles from './Step2ConfirmationDialog.scss';
import ReactToolboxMobxForm, {
  handleFormErrors,
} from '../../../utils/ReactToolboxMobxForm';
import { formattedWalletAmount } from '../../../utils/formatters';
import { isValidSpendingPassword } from '../../../utils/validations';
import { submitOnEnter } from '../../../utils/form';
import globalMessages from '../../../i18n/global-messages';
import LocalizableError from '../../../i18n/LocalizableError';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';

const messages = defineMessages({
  title: {
    id: 'staking.redeemItnRewards.step2.title',
    defaultMessage: '!!!Confirm rewards redemption',
    description: 'title for Redeem Incentivized Testnet - Step 2',
  },
  walletToLabel: {
    id: 'staking.redeemItnRewards.step2.walletToLabel',
    defaultMessage: '!!!To',
    description: 'walletToLabel for Redeem Incentivized Testnet - Step 2',
  },
  walletToName: {
    id: 'staking.redeemItnRewards.step2.walletToName',
    defaultMessage: '!!!<b>{walletName}</b> wallet',
    description: 'walletToName for Redeem Incentivized Testnet - Step 2',
  },
  rewardsTotal: {
    id: 'staking.redeemItnRewards.step2.rewardsTotal',
    defaultMessage: '!!!Rewards total',
    description: 'rewardsTotal for Redeem Incentivized Testnet - Step 2',
  },
  transactionFees: {
    id: 'staking.redeemItnRewards.step2.transactionFees',
    defaultMessage: '!!!Transaction fees',
    description: 'transactionFees for Redeem Incentivized Testnet - Step 2',
  },
  finalTotal: {
    id: 'staking.redeemItnRewards.step2.finalTotal',
    defaultMessage: '!!!Final total',
    description: 'finalTotal for Redeem Incentivized Testnet - Step 2',
  },
  spendingPasswordLabel: {
    id: 'staking.redeemItnRewards.step2.spendingPasswordLabel',
    defaultMessage:
      '<span>Wallet spending password</span> (<b>{walletName}</b> wallet)',
    description:
      'spendingPasswordLabel for Redeem Incentivized Testnet - Step 2',
  },
  spendingPasswordPlaceholder: {
    id: 'staking.redeemItnRewards.step2.spendingPasswordPlaceholder',
    defaultMessage: 'Password',
    description:
      'spendingPasswordPlaceholder for Redeem Incentivized Testnet - Step 2',
  },
  continueButtonLabel: {
    id: 'staking.redeemItnRewards.step2.continueButtonLabel',
    defaultMessage: 'Confirm rewards redemption',
    description: 'continueButtonLabel for Redeem Incentivized Testnet - Step 2',
  },
});

type Props = {
  walletName: string,
  rewardsTotal: number,
  transactionFees: number,
  finalTotal: number,
  onContinue: Function,
  onClose: Function,
  onBack: Function,
  isSubmitting: boolean,
  error?: ?LocalizableError,
};

@observer
export default class Step2ConfirmationDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static defaultProps = {
    error: null,
  };

  // componentDidUpdate() {
  //   if (this.props.error) {
  //     handleFormErrors('.ConfigurationDialog_error');
  //   }
  // }

  form = new ReactToolboxMobxForm(
    {
      fields: {
        spendingPassword: {
          type: 'password',
          label: (
            <FormattedHTMLMessage
              {...messages.spendingPasswordLabel}
              values={{
                walletName: this.props.walletName,
              }}
            />
          ),
          placeholder: this.context.intl.formatMessage(
            messages.spendingPasswordPlaceholder
          ),
          value: '',
          validators: [
            ({ field }) => {
              return [
                isValidSpendingPassword(field.value),
                this.context.intl.formatMessage(
                  globalMessages.invalidSpendingPassword
                ),
              ];
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

  submit = () => {
    this.form.submit({
      onSuccess: form => {
        const { spendingPassword } = form.values();
        const { onContinue } = this.props;
        onContinue({ spendingPassword });
      },
      onError: () =>
        handleFormErrors('.Step2ConfirmationDialog_error', {
          focusElement: true,
        }),
    });
  };

  handleSubmitOnEnter = submitOnEnter.bind(this, this.submit);

  render() {
    const { intl } = this.context;
    const { form } = this;
    const {
      walletName,
      rewardsTotal,
      transactionFees,
      finalTotal,
      onContinue,
      onClose,
      onBack,
      isSubmitting,
      error,
    } = this.props;

    const spendingPasswordField = form.$('spendingPassword');

    console.log('walletName', walletName);
    console.log('rewardsTotal', rewardsTotal);
    console.log('transactionFees', transactionFees);
    console.log('finalTotal', finalTotal);
    console.log('error', error);

    return (
      <Dialog
        title={intl.formatMessage(messages.title)}
        actions={[
          {
            className: isSubmitting ? styles.isSubmitting : null,
            disabled: !form.isValid,
            primary: true,
            label: intl.formatMessage(messages.continueButtonLabel),
            onClick: this.submit,
          },
        ]}
        onContinue={onContinue}
        onClose={onClose}
        onBack={onBack}
        closeButton={<DialogCloseButton />}
      >
        <div className={styles.component}>
          <div className={styles.sectionTo}>
            <div className={styles.sectionLabel}>
              {intl.formatMessage(messages.walletToLabel)}
            </div>
            <div className={styles.sectionStringValue}>
              <FormattedHTMLMessage
                {...messages.walletToName}
                values={{
                  walletName,
                }}
              />
            </div>
          </div>
          <div className={styles.sectionRewardsTotal}>
            <div className={styles.sectionLabel}>
              {intl.formatMessage(messages.rewardsTotal)}
            </div>
            <div className={styles.sectionStringValue}>
              {/* formattedWalletAmount(rewardsTotal) */}
              {rewardsTotal}
            </div>
          </div>
          <div className={styles.sectionTransactionFees}>
            <div className={styles.sectionLabel}>
              {intl.formatMessage(messages.transactionFees)}
            </div>
            <div className={styles.sectionStringValue}>
              {/* formattedWalletAmount(transactionFees) */}
              {transactionFees}
            </div>
          </div>
          <div className={styles.sectionFinalTotal}>
            <div className={styles.sectionLabel}>
              {intl.formatMessage(messages.finalTotal)}
            </div>
            <div className={styles.sectionStringValue}>
              {/* formattedWalletAmount(finalTotal) */}
              {finalTotal}
            </div>
          </div>

          <Input
            className={styles.spendingPassword}
            {...spendingPasswordField.bind()}
            skin={InputSkin}
            error={spendingPasswordField.error}
            onKeyPress={this.handleSubmitOnEnter}
          />
          {error && <p className={styles.error}>{intl.formatMessage(error)}</p>}
        </div>
      </Dialog>
    );
  }
}
