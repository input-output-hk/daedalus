// @flow
/* eslint-disable jsx-a11y/label-has-associated-control, jsx-a11y/label-has-for */
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import BigNumber from 'bignumber.js';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import classnames from 'classnames';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { Input } from 'react-polymorph/lib/components/Input';
import { CheckboxSkin } from 'react-polymorph/lib/skins/simple/CheckboxSkin';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import { formattedWalletAmount } from '../../../utils/formatters';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import { FormattedHTMLMessageWithLink } from '../../widgets/FormattedHTMLMessageWithLink';
import Dialog from '../../widgets/Dialog';
import styles from './UndelegateConfirmationDialog.scss';
import globalMessages from '../../../i18n/global-messages';
import LocalizableError from '../../../i18n/LocalizableError';
import { submitOnEnter } from '../../../utils/form';

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
  description: {
    id: 'staking.delegationCenter.undelegate.dialog.description',
    defaultMessage:
      '!!!<p>The stake from your wallet <strong>{walletName}</strong> is currently delegated to the <strong>[{stakePoolSlug}] {stakePoolName}</strong> stake pool.</p><p>Do you want to undelegate your stake and stop earning rewards?</p>',
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
});

type Props = {
  walletName: string,
  stakePoolName: string,
  stakePoolSlug: string,
  onConfirm: Function,
  onCancel: Function,
  onExternalLinkClick: Function,
  isSubmitting: boolean,
  error: ?LocalizableError,
  fees: BigNumber,
};

type State = {
  isConfirmUnsupportChecked: boolean,
  isConfirmUneligibleChecked: boolean,
  passphrase: string,
};

@observer
export default class UndelegateConfirmationDialog extends Component<
  Props,
  State
> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  constructor(props: Props) {
    super(props);

    this.state = {
      isConfirmUnsupportChecked: false,
      isConfirmUneligibleChecked: false,
      passphrase: '',
    };
  }

  onConfirmUnsupportCheckChange = () =>
    this.setState(prevState => ({
      isConfirmUnsupportChecked: !prevState.isConfirmUnsupportChecked,
    }));

  onConfirmUneligibleCheckChange = () =>
    this.setState(prevState => ({
      isConfirmUneligibleChecked: !prevState.isConfirmUneligibleChecked,
    }));

  onPassphraseChange = (passphrase: string) => this.setState({ passphrase });

  isConfirmDisabled = () => {
    const { isSubmitting } = this.props;
    const {
      isConfirmUnsupportChecked,
      isConfirmUneligibleChecked,
      passphrase,
    } = this.state;

    return (
      isSubmitting ||
      !isConfirmUnsupportChecked ||
      !isConfirmUneligibleChecked ||
      !passphrase
    );
  };

  handleSubmit = () => {
    if (this.isConfirmDisabled()) {
      return false;
    }

    const { onConfirm } = this.props;
    const { passphrase } = this.state;

    return onConfirm(passphrase);
  };

  handleSubmitOnEnter = (event: KeyboardEvent) =>
    submitOnEnter(this.handleSubmit, event);

  generateErrorElement = () => {
    const { error, onExternalLinkClick } = this.props;

    if (!error) {
      return null;
    }

    const errorHasLink = !!error.values.linkLabel;
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
    const { intl } = this.context;
    const {
      walletName,
      stakePoolName,
      stakePoolSlug,
      onCancel,
      onConfirm,
      isSubmitting,
      fees,
    } = this.props;
    const {
      isConfirmUnsupportChecked,
      isConfirmUneligibleChecked,
      passphrase,
    } = this.state;
    const isConfirmDisabled = this.isConfirmDisabled();
    const buttonClasses = classnames([
      'attention',
      isSubmitting ? styles.isSubmitting : null,
    ]);
    const actions = [
      {
        label: intl.formatMessage(globalMessages.cancel),
        onClick: onCancel,
      },
      {
        className: buttonClasses,
        label: intl.formatMessage(messages.confirmButtonLabel),
        onClick: () => onConfirm(passphrase),
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
        onClose={onCancel}
        className={styles.dialog}
        closeButton={<DialogCloseButton onClose={onCancel} />}
      >
        <div className={styles.description}>
          <FormattedHTMLMessage
            {...messages.description}
            values={{ walletName, stakePoolName, stakePoolSlug }}
          />
        </div>
        <Checkbox
          label={intl.formatMessage(messages.confirmUnsupportCheck)}
          onChange={this.onConfirmUnsupportCheckChange}
          checked={isConfirmUnsupportChecked}
          skin={CheckboxSkin}
        />
        <Checkbox
          label={intl.formatMessage(messages.confirmUneligibleCheck)}
          onChange={this.onConfirmUneligibleCheckChange}
          checked={isConfirmUneligibleChecked}
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
          label={intl.formatMessage(messages.spendingPasswordLabel)}
          placeholder={intl.formatMessage(messages.spendingPasswordPlaceholder)}
          value={passphrase}
          onKeyPress={this.handleSubmitOnEnter}
          onChange={this.onPassphraseChange}
          skin={InputSkin}
        />
        {errorElement && <p className={styles.error}>{errorElement}</p>}
      </Dialog>
    );
  }
}
