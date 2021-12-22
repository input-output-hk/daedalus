import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import BigNumber from 'bignumber.js';
import { defineMessages, intlShape, FormattedMessage } from 'react-intl';
import vjf from 'mobx-react-form/lib/validators/VJF';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import DialogBackButton from '../../widgets/DialogBackButton';
import Dialog from '../../widgets/Dialog';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './TransferFundsStep2Dialog.scs... Remove this comment to see the full error message
import styles from './TransferFundsStep2Dialog.scss';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';
import globalMessages from '../../../i18n/global-messages';
import LocalizableError from '../../../i18n/LocalizableError';
import { submitOnEnter } from '../../../utils/form';
import { formattedWalletAmount } from '../../../utils/formatters';
import { DECIMAL_PLACES_IN_ADA } from '../../../config/numbersConfig';

const messages = defineMessages({
  dialogTitle: {
    id: 'wallet.transferFunds.dialog2.title',
    defaultMessage: '!!!Transfer funds from the legacy wallet',
    description: 'Title in the transfer funds form.',
  },
  description: {
    id: 'wallet.transferFunds.dialog2.description.label',
    defaultMessage:
      '!!!Confirm transfer from {sourceWalletName}wallet to the {targetWalletName} wallet.',
    description: 'description in the transfer funds form.',
  },
  sourceWalletAmountLabel: {
    id: 'wallet.transferFunds.dialog2.sourceWalletAmount.label',
    defaultMessage: '!!!{sourceWalletName} amount',
    description: 'Label Source wallet Amount in the transfer funds form',
  },
  feesLabel: {
    id: 'wallet.transferFunds.dialog2.fees.label',
    defaultMessage: '!!!Fees',
    description: 'Label Fees in the transfer funds form',
  },
  totalLabel: {
    id: 'wallet.transferFunds.dialog2.total.label',
    defaultMessage: '!!!Total',
    description: 'Total Fees in the transfer funds form',
  },
  leftoversLabel: {
    id: 'wallet.transferFunds.dialog2.leftovers.label',
    defaultMessage: '!!!Leftovers',
    description: 'Label Leftovers in the transfer funds form',
  },
  leftoversLearnMoreLabel: {
    id: 'wallet.transferFunds.dialog2.leftovers.LearnMore.label',
    defaultMessage: '!!!Learn more',
    description: 'Label Leftovers in the transfer funds form',
  },
  leftoversLearnMoreUrl: {
    id: 'wallet.transferFunds.dialog2.leftovers.LearnMore.url',
    defaultMessage: '!!!https://iohk.zendesk.com/hc/en-us/articles/',
    description: 'Label Leftovers in the transfer funds form',
  },
  buttonLabel: {
    id: 'wallet.transferFunds.dialog2.label.buttonLabel',
    defaultMessage: '!!!Transfer funds',
    description: 'buttonLabel in the transfer funds form.',
  },
  passphraseFieldPlaceholder: {
    id: 'wallet.transferFunds.dialog2.passphraseFieldPlaceholder',
    defaultMessage: '!!!Spending password',
    description: 'passphraseFieldPlaceholder in the transfer funds form.',
  },
  passphraseLabel: {
    id: 'wallet.transferFunds.dialog2.passphraseLabel',
    defaultMessage: '!!!Spending password',
    description: 'passphraseLabel in the transfer funds form.',
  },
});
messages.fieldIsRequired = globalMessages.fieldIsRequired;
type Props = {
  onFinish: (...args: Array<any>) => any;
  onClose: (...args: Array<any>) => any;
  onBack: (...args: Array<any>) => any;
  onOpenExternalLink: (...args: Array<any>) => any;
  feesAmount: BigNumber;
  leftoversAmount: BigNumber;
  sourceWalletAmount: BigNumber;
  sourceWalletName: string;
  targetWalletName: string;
  isSubmitting?: boolean;
  error?: LocalizableError | null | undefined;
};

@observer
class TransferFundsStep2Dialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  form = new ReactToolboxMobxForm(
    // @ts-ignore ts-migrate(2554) FIXME: Expected 0 arguments, but got 2.
    {
      fields: {
        spendingPassword: {
          type: 'password',
          label: this.context.intl.formatMessage(messages.passphraseLabel),
          placeholder: this.context.intl.formatMessage(
            messages.passphraseFieldPlaceholder
          ),
          value: '',
          validators: [
            ({ field }) => {
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
        const { spendingPassword } = form.values();
        this.props.onFinish(spendingPassword);
      },
      onError: () => {},
    });
  };
  handleSubmitOnEnter = (event: KeyboardEvent) =>
    // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
    this.form.$('spendingPassword').isValid &&
    submitOnEnter(this.submit, event);

  render() {
    const { intl } = this.context;
    const {
      onClose,
      onBack,
      feesAmount,
      leftoversAmount,
      sourceWalletName,
      sourceWalletAmount,
      targetWalletName,
      onOpenExternalLink,
      isSubmitting,
      error,
    } = this.props;
    const fees = feesAmount.toFormat(DECIMAL_PLACES_IN_ADA);
    const leftovers =
      leftoversAmount && !leftoversAmount.isZero()
        ? leftoversAmount.toFormat(DECIMAL_PLACES_IN_ADA)
        : null;
    const totalAmount = sourceWalletAmount
      .minus(feesAmount)
      .minus(leftoversAmount);
    const totalToBeReceived = formattedWalletAmount(totalAmount, false);
    const sourceWalletBalance = formattedWalletAmount(
      sourceWalletAmount,
      false
    );
    // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
    const spendingPasswordField = this.form.$('spendingPassword');
    const buttonClasses = classnames([
      'confirmButton',
      isSubmitting ? styles.submitButtonSpinning : null,
    ]);
    const actions = [
      {
        label: intl.formatMessage(messages.buttonLabel),
        onClick: this.submit,
        primary: true,
        className: buttonClasses,
        disabled: isSubmitting || !spendingPasswordField.isValid,
      },
    ];
    return (
      <Dialog
        className={styles.dialog}
        title={intl.formatMessage(messages.dialogTitle)}
        actions={actions}
        closeOnOverlayClick
        onClose={onClose}
        closeButton={<DialogCloseButton />}
        backButton={<DialogBackButton onBack={onBack} />}
      >
        <FormattedMessage
          {...messages.description}
          values={{
            sourceWalletName: <b key="source">{sourceWalletName}</b>,
            targetWalletName: <b key="target">{targetWalletName}</b>,
          }}
        >
          {(...content) => <div className={styles.description}>{content}</div>}
        </FormattedMessage>
        <div className={styles.amountGroupFull}>
          <p className={styles.label}>
            <FormattedMessage
              {...messages.sourceWalletAmountLabel}
              values={{
                sourceWalletName: <b key="source">{sourceWalletName}</b>,
              }}
            />
          </p>
          <div className={styles.amount}>{sourceWalletBalance}</div>
        </div>
        <div className={styles.amountGroup}>
          <p className={styles.label}>
            {intl.formatMessage(messages.feesLabel)}
          </p>
          <div className={styles.amountOpacity}>{fees}</div>
        </div>
        {leftovers && (
          <div className={styles.amountGroup}>
            <p className={styles.label}>
              {intl.formatMessage(messages.leftoversLabel)}
              <Link
                className={styles.leftoversLearnMoreLink}
                onClick={(event: React.MouseEvent<HTMLElement>) =>
                  onOpenExternalLink(
                    intl.formatMessage(messages.leftoversLearnMoreUrl, event)
                  )
                }
                label={intl.formatMessage(messages.leftoversLearnMoreLabel)}
                skin={LinkSkin}
              />
            </p>
            <div className={styles.amountOpacity}>{leftovers}</div>
          </div>
        )}
        <div className={styles.amountGroupFull}>
          <p className={styles.label}>
            {intl.formatMessage(messages.totalLabel)}
          </p>
          <div className={styles.amount}>{totalToBeReceived}</div>
        </div>
        <Input
          type="password"
          className={styles.currentPassword}
          {...spendingPasswordField.bind()}
          error={spendingPasswordField.error}
          skin={InputSkin}
          onKeyPress={this.handleSubmitOnEnter}
          autoFocus
        />
        {error ? (
          <p className={styles.error}>
            {this.context.intl.formatMessage(error)}
          </p>
        ) : null}
      </Dialog>
    );
  }
}

export default TransferFundsStep2Dialog;
