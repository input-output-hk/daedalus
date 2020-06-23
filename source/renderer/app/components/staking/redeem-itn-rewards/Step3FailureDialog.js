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
// import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';

const messages = defineMessages({
  title: {
    id: 'staking.redeemItnRewards.step2.title',
    defaultMessage: '!!!Confirm rewards redemption',
    description: 'Title for Redeem Incentivized Testnet - Step 2',
  },
});

type Props = {
  onClose: Function,
  onBack: Function,
};

@observer
export default class Step3FailureDialog extends Component<Props> {
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

  // form = new ReactToolboxMobxForm(
  //   {
  //     fields: {
  //       walletName: {
  //         label: this.context.intl.formatMessage(messages.walletNameLabel),
  //         placeholder: this.context.intl.formatMessage(
  //           messages.walletNamePlaceholder
  //         ),
  //         value: this.props.walletName,
  //         validators: [
  //           ({ field }) => [
  //             isValidWalletName(field.value),
  //             this.context.intl.formatMessage(globalMessages.invalidWalletName),
  //           ],
  //         ],
  //         hooks: {
  //           onChange: this.props.onChange.bind(this, 'walletName'),
  //         },
  //       },
  //     },
  //   },
  // );

  // submit = () => {
  //   this.form.submit({
  //     onSuccess: form => {
  //       const { onContinue } = this.props;
  //       onContinue(...);
  //     },
  //     onError: () =>
  //       handleFormErrors('.ConfigurationDialog_error', { focusElement: true }),
  //   });
  // };

  // handleSubmitOnEnter = submitOnEnter.bind(this, this.submit);

  // resetForm = () => {
  //   const { form } = this;
  //   // Cancel all debounced field validations
  //   form.each(field => {
  //     field.debouncedValidation.cancel();
  //   });
  //   form.reset();
  //   form.showErrors(false);
  // };

  render() {
    const { intl } = this.context;
    const { onClose, onBack } = this.props;

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

  // render() {
  //   const { intl } = this.context;
  //   const { onClose, onBack, error, isSubmitting } = this.props;
  //   const { form } = this;
  //   const canSubmit = !isSubmitting && form.isValid;
  //   return (
  //     <Dialog
  //       title={'Redeem Incentivized Testnet rewards'}
  //       actions={[
  //         {
  //           className: isSubmitting ? styles.isSubmitting : null,
  //           disabled: !canSubmit,
  //           primary: true,
  //           label: intl.formatMessage(messages.continueButtonLabel),
  //           onClick: this.submit,
  //         },
  //       ]}
  //       onClose={onClose}
  //       onBack={onBack}
  //     >
  //       <div className={styles.component}>
  //         {error && <p className={styles.error}>{intl.formatMessage(error)}</p>}
  //       </div>
  //     </Dialog>
  //   );
  // }
}
