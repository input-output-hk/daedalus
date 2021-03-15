// @flow
import React, { Component } from 'react';
import BigNumber from 'bignumber.js';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import vjf from 'mobx-react-form/lib/validators/VJF';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import styles from './Step2ConfirmationDialog.scss';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import Wallet from '../../../domains/Wallet';
import { formattedWalletAmount } from '../../../utils/formatters';
import { isValidSpendingPassword } from '../../../utils/validations';
import { submitOnEnter } from '../../../utils/form';
import globalMessages from '../../../i18n/global-messages';
import LocalizableError from '../../../i18n/LocalizableError';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';
import { MIN_REWARDS_REDEMPTION_RECEIVER_BALANCE } from '../../../config/stakingConfig';

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
    defaultMessage: '!!!{walletName} <span>wallet</span>',
    description: 'walletToName for Redeem Incentivized Testnet - Step 2',
  },
  transactionFees: {
    id: 'staking.redeemItnRewards.step2.transactionFees',
    defaultMessage: '!!!Transaction fees',
    description: 'transactionFees for Redeem Incentivized Testnet - Step 2',
  },
  spendingPasswordLabel: {
    id: 'staking.redeemItnRewards.step2.spendingPasswordLabel',
    defaultMessage:
      '!!!Wallet spending password <em>(<b>{walletName}</b> wallet)</em>',
    description:
      'spendingPasswordLabel for Redeem Incentivized Testnet - Step 2',
  },
  spendingPasswordPlaceholder: {
    id: 'staking.redeemItnRewards.step2.spendingPasswordPlaceholder',
    defaultMessage: '!!!Password',
    description:
      'spendingPasswordPlaceholder for Redeem Incentivized Testnet - Step 2',
  },
  continueButtonLabel: {
    id: 'staking.redeemItnRewards.step2.continueButtonLabel',
    defaultMessage: '!!!Confirm rewards redemption',
    description: 'continueButtonLabel for Redeem Incentivized Testnet - Step 2',
  },
  backButtonLabel: {
    id: 'staking.redeemItnRewards.step2.backButtonLabel',
    defaultMessage: '!!!Back',
    description:
      'Label for the back button in the wallet send confirmation dialog.',
  },
});

type Props = {
  wallet: Wallet,
  transactionFees: BigNumber,
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

  form = new ReactToolboxMobxForm(
    {
      fields: {
        spendingPassword: {
          type: 'password',
          label: (
            <FormattedHTMLMessage
              {...messages.spendingPasswordLabel}
              values={{
                walletName: this.props.wallet.name,
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
      onSuccess: (form) => {
        const { spendingPassword } = form.values();
        const { onContinue } = this.props;
        onContinue({ spendingPassword });
      },
    });
  };

  handleSubmitOnEnter = submitOnEnter.bind(this, this.submit);

  render() {
    const { intl } = this.context;
    const { form } = this;
    const {
      wallet,
      transactionFees,
      onContinue,
      onClose,
      onBack,
      isSubmitting,
      error,
    } = this.props;

    const { amount } = wallet || {};

    const minRewardsReceiverBalance = new BigNumber(
      MIN_REWARDS_REDEMPTION_RECEIVER_BALANCE
    );
    const differenceBetweenAmountAndFee = amount.minus(transactionFees);
    const calculatedTransactionFees = differenceBetweenAmountAndFee.isLessThan(
      minRewardsReceiverBalance
    )
      ? amount
      : transactionFees;

    const { name: walletName } = wallet;

    const spendingPasswordField = form.$('spendingPassword');

    const actions = {
      direction: 'column',
      items: [
        {
          className: isSubmitting ? styles.isSubmitting : null,
          disabled: !form.isValid,
          primary: true,
          label: intl.formatMessage(messages.continueButtonLabel),
          onClick: this.submit,
        },
        {
          onClick: onBack,
          label: intl.formatMessage(messages.backButtonLabel),
          isLink: true,
          hasIconAfter: false,
        },
      ],
    };

    const closeButton = <DialogCloseButton onClose={onClose} />;

    return (
      <Dialog
        title={intl.formatMessage(messages.title)}
        actions={actions}
        onContinue={onContinue}
        onClose={onClose}
        closeButton={closeButton}
        onBack={onBack}
        closeOnOverlayClick={false}
        fullSize
      >
        <div className={styles.to}>
          <div>{intl.formatMessage(messages.walletToLabel)}</div>
          <div>
            <FormattedHTMLMessage
              {...messages.walletToName}
              values={{
                walletName,
              }}
            />
          </div>
        </div>
        <div className={styles.transactionFees}>
          <div>{intl.formatMessage(messages.transactionFees)}</div>
          <div>
            <b>
              {formattedWalletAmount(calculatedTransactionFees, false)}&nbsp;
            </b>
            <em>{intl.formatMessage(globalMessages.unitAda)}</em>
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
      </Dialog>
    );
  }
}
