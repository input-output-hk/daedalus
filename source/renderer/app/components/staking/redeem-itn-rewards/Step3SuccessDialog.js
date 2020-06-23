// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
// import classnames from 'classnames';
// import { Input } from 'react-polymorph/lib/components/Input';
// import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import {
  defineMessages,
  intlShape /* FormattedHTMLMessage */,
} from 'react-intl';
// import vjf from 'mobx-react-form/lib/validators/VJF';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import styles from './Step1ConfigurationDialog.scss';
// import ReactToolboxMobxForm, {
//   handleFormErrors,
// } from '../../../utils/ReactToolboxMobxForm';
// import {
//   isValidWalletName,
//   isValidSpendingPassword,
//   isValidRepeatPassword,
// } from '../../../utils/validations';
// import { submitOnEnter } from '../../../utils/form';
// import globalMessages from '../../../i18n/global-messages';
import LocalizableError from '../../../i18n/LocalizableError';
// import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';

const messages = defineMessages({
  title: {
    id: 'staking.redeemItnRewards.step3.success.title',
    defaultMessage: '!!!Incentivized Testnet rewards redeemed!',
    description: 'Title for Redeem Incentivized Testnet - Step 2',
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
  error?: ?LocalizableError,
};

@observer
export default class Step3SuccessDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static defaultProps = {
    error: null,
  };

  render() {
    const { intl } = this.context;
    const {
      walletName,
      rewardsTotal,
      transactionFees,
      finalTotal,
      isSubmitting,
      onContinue,
      onClose,
      onBack,
      error,
    } = this.props;

    console.log('walletName', walletName);
    console.log('rewardsTotal', rewardsTotal);
    console.log('transactionFees', transactionFees);
    console.log('finalTotal', finalTotal);
    console.log('isSubmitting', isSubmitting);
    console.log('onContinue', onContinue);
    console.log('onClose', onClose);
    console.log('onBack', onBack);
    console.log('error', error);

    return (
      <Dialog
        title={intl.formatMessage(messages.title)}
        actions={[
          {
            // className: isSubmitting ? styles.isSubmitting : null,
            // disabled: !canSubmit,
            primary: true,
            label: '!!!Continue',
            // label: intl.formatMessage(messages.continueButtonLabel),
            // onClick: this.submit,
          },
        ]}
        onClose={onClose}
        onBack={onBack}
        closeButton={<DialogCloseButton />}
      >
        {/* <div className={styles.component}>
           {error && <p className={styles.error}>{intl.formatMessage(error)}</p>}
         </div> */}
      </Dialog>
    );
  }
}
