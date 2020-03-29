// @flow
/* eslint-disable jsx-a11y/label-has-associated-control, jsx-a11y/label-has-for */
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { get } from 'lodash';
import BigNumber from 'bignumber.js';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import classnames from 'classnames';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { Input } from 'react-polymorph/lib/components/Input';
import { CheckboxSkin } from 'react-polymorph/lib/skins/simple/CheckboxSkin';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';
import { formattedWalletAmount } from '../../../utils/formatters';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import { FormattedHTMLMessageWithLink } from '../../widgets/FormattedHTMLMessageWithLink';
import Dialog from '../../widgets/Dialog';
import globalMessages from '../../../i18n/global-messages';
import LocalizableError from '../../../i18n/LocalizableError';
import { submitOnEnter } from '../../../utils/form';
import styles from './UndelegateConfirmationDialog.scss';

const messages = defineMessages({
  dialogTitle: {
    id: 'staking.delegationCenter.undelegate.dialog.title',
    defaultMessage: '!!!Undelegate',
    description: 'Title for the "Undelegate" dialog.',
  },
  confirmButtonLabel: {
    id: 'staking.delegationCenter.undelegate.dialog.confirmButtonLabel',
    defaultMessage: '!!!Undelegate',
    description:
      'Label for the "Undelegate" button in the "Undelegate" dialog.',
  },
  descriptionWithTicker: {
    id: 'staking.delegationCenter.undelegate.dialog.descriptionWithTicker',
    defaultMessage:
      '!!!<p>The stake from your wallet <strong>{walletName}</strong> is currently delegated to the <strong>[{stakePoolTicker}] {stakePoolName}</strong> stake pool.</p><p>Do you want to undelegate your stake and stop earning rewards?</p>',
    description: 'Description for the "Undelegate" dialog.',
  },
  descriptionWithUnknownTicker: {
    id:
      'staking.delegationCenter.undelegate.dialog.descriptionWithUnknownTicker',
    defaultMessage:
      '!!!<p>The stake from your wallet <strong>{walletName}</strong> is currently delegated to the <strong>{stakePoolTicker}</strong> stake pool.</p><p>Do you want to undelegate your stake and stop earning rewards?</p>',
    description: 'Description for the "Undelegate" dialog.',
  },
  confirmUnsupportCheck: {
    id: 'staking.delegationCenter.undelegate.dialog.confirmUnsupportCheck',
    defaultMessage:
      '!!!I understand that I am not supporting the Cardano network when my stake is undelegated.',
    description:
      'Label for the unsupport confirmation check in the "Undelegate" dialog.',
  },
  confirmUneligibleCheck: {
    id: 'staking.delegationCenter.undelegate.dialog.confirmUneligibleCheck',
    defaultMessage:
      '!!!I understand that I will not be eligible to earn rewards when my stake is undelegated.',
    description:
      'Label for the uneligible confirmation check in the "Undelegate" dialog.',
  },
  feesLabel: {
    id: 'staking.delegationCenter.undelegate.dialog.feesLabel',
    defaultMessage: '!!!Fees',
    description: 'Fees label in the "Undelegate" dialog.',
  },
  adaLabel: {
    id: 'staking.delegationCenter.undelegate.dialog.adaLabel',
    defaultMessage: '!!!ADA',
    description: 'ADA label in the "Undelegate" dialog.',
  },
  spendingPasswordLabel: {
    id: 'staking.delegationCenter.undelegate.dialog.spendingPasswordLabel',
    defaultMessage: '!!!Spending password',
    description: 'Spending password label in the "Undelegate" dialog.',
  },
  spendingPasswordPlaceholder: {
    id:
      'staking.delegationCenter.undelegate.dialog.spendingPasswordPlaceholder',
    defaultMessage: '!!!Type your spending password here',
    description: 'Spending password placeholder in the "Undelegate" dialog.',
  },
  passwordErrorMessage: {
    id: 'staking.delegationCenter.undelegate.dialog.passwordError',
    defaultMessage: '!!!Incorrect spending password.',
    description: 'Label for password error in the "Undelegate" dialog.',
  },
  unknownStakePoolLabel: {
    id: 'staking.delegationCenter.undelegate.dialog.unknownStakePoolLabel',
    defaultMessage: '!!!unknown',
    description: 'unknown stake pool label in the "Undelegate" dialog.',
  },
});

messages.fieldIsRequired = globalMessages.fieldIsRequired;

type Props = {
  walletName: string,
  stakePoolName: ?string,
  stakePoolTicker: ?string,
  onConfirm: Function,
  onCancel: Function,
  onExternalLinkClick: Function,
  isSubmitting: boolean,
  error: ?LocalizableError,
  fees: BigNumber,
};

@observer
export default class UndelegateConfirmationDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  form = new ReactToolboxMobxForm(
    {
      fields: {
        isConfirmUnsupportChecked: {
          type: 'checkbox',
          label: this.context.intl.formatMessage(
            messages.confirmUnsupportCheck
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
        isConfirmUneligibleChecked: {
          type: 'checkbox',
          label: this.context.intl.formatMessage(
            messages.confirmUneligibleCheck
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
      options: {
        validateOnChange: true,
        validationDebounceWait: FORM_VALIDATION_DEBOUNCE_WAIT,
      },
    }
  );

  isConfirmDisabled = () => {
    const { form } = this;
    const { isSubmitting } = this.props;
    const { isValid: unsupportCheckboxIsValid } = form.$(
      'isConfirmUnsupportChecked'
    );
    const { isValid: uneligibleCheckboxIsValid } = form.$(
      'isConfirmUneligibleChecked'
    );
    const { isValid: passphraseIsValid } = form.$('passphrase');

    return (
      isSubmitting ||
      !unsupportCheckboxIsValid ||
      !uneligibleCheckboxIsValid ||
      !passphraseIsValid
    );
  };

  handleSubmit = () => {
    if (this.isConfirmDisabled()) {
      return false;
    }

    return this.form.submit({
      onSuccess: form => {
        const { onConfirm } = this.props;
        const { passphrase } = form.values();
        onConfirm(passphrase);
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
    const unsupportCheckboxField = form.$('isConfirmUnsupportChecked');
    const uneligibleCheckboxField = form.$('isConfirmUneligibleChecked');
    const passphraseField = form.$('passphrase');
    const {
      walletName,
      stakePoolName,
      stakePoolTicker,
      onCancel,
      isSubmitting,
      fees,
    } = this.props;
    const isConfirmDisabled = this.isConfirmDisabled();
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
        disabled: isConfirmDisabled,
        primary: true,
      },
    ];
    const errorElement = this.generateErrorElement();

    return (
      <Dialog
        title={intl.formatMessage(messages.dialogTitle)}
        actions={actions}
        closeOnOverlayClick
        onClose={!isSubmitting ? onCancel : () => null}
        className={styles.dialog}
        closeButton={<DialogCloseButton />}
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
          skin={CheckboxSkin}
        />
        <Checkbox
          {...uneligibleCheckboxField.bind()}
          error={uneligibleCheckboxField.error}
          skin={CheckboxSkin}
        />
        <div className={styles.divider} />
        <div className={styles.feesWrapper}>
          <label className="SimpleFormField_label">
            {intl.formatMessage(messages.feesLabel)}
          </label>
          <p className={styles.feesAmount}>
            <span>{formattedWalletAmount(fees, false)}</span>
            <span className={styles.feesAmountLabel}>
              &nbsp;{intl.formatMessage(messages.adaLabel)}
            </span>
          </p>
        </div>
        <Input
          type="password"
          {...passphraseField.bind()}
          error={passphraseField.error}
          skin={InputSkin}
          onKeyPress={this.handleSubmitOnEnter}
        />
        {errorElement && <p className={styles.error}>{errorElement}</p>}
      </Dialog>
    );
  }
}
