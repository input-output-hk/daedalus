import React, { Component } from 'react';
// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'Node'.
import type { Node } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import vjf from 'mobx-react-form/lib/validators/VJF';
// import { Input } from 'react-polymorph/lib/components/Input';
// import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import globalMessages from '../../../i18n/global-messages';
import Dialog from '../../widgets/Dialog';
import LocalizableError from '../../../i18n/LocalizableError';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './ExportWalletToFileDialog.scs... Remove this comment to see the full error message
import styles from './ExportWalletToFileDialog.scss';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';
import { submitOnEnter } from '../../../utils/form';

const messages = defineMessages({
  headline: {
    id: 'wallet.settings.exportToFile.dialog.headline',
    defaultMessage: '!!!Export Wallet',
    description: 'headline for "export wallet to file" dialog.',
  },
  introduction: {
    id: 'wallet.settings.exportToFile.dialog.introduction',
    defaultMessage:
      '!!!You are exporting <strong>{walletName}</strong> to a file.',
    description: 'headline for "export wallet to file" dialog.',
  },
  exportButtonLabel: {
    id: 'wallet.settings.exportToFile.dialog.submit.label',
    defaultMessage: '!!!Export',
    description: 'Label for export wallet to file submit button.',
  }, // TODO: re-enable when we have full/readOnly exports
  // fullTabTitle: {
  //   id: 'wallet.export.choices.tab.title.full',
  //   defaultMessage: '!!!Full',
  //   description: 'Tab title "Full" on wallet export dialog.'
  // },
  // readOnlyTabTitle: {
  //   id: 'wallet.export.choices.tab.title.readOnly',
  //   defaultMessage: '!!!Read-only',
  //   description: 'Tab title "Read-only" on wallet export dialog.'
  // },
});
type ExportType = 'full' | 'readOnly';
const EXPORT_TYPE = {
  FULL: 'full',
  READ_ONLY: 'readOnly',
};
export type OnSubmitParams = {
  exportType: ExportType;
  password: string | null | undefined;
};
type Props = {
  walletName: string;
  isSubmitting: boolean;
  onSubmit: (arg0: OnSubmitParams) => Promise<void>;
  onClose: () => void;
  error?: LocalizableError | null | undefined;
};
type State = {
  exportType: ExportType;
};

@observer
class ExportWalletToFileDialog extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  constructor(props: Props, children: Node) {
    super(props, children);
    this.state = {
      // @ts-ignore ts-migrate(2322) FIXME: Type 'string' is not assignable to type 'ExportTyp... Remove this comment to see the full error message
      exportType: EXPORT_TYPE.FULL,
    };
  }

  // onChangeExportType(exportType: ExportType) {
  //   this.setState({ exportType });
  // }
  form = new ReactToolboxMobxForm(
    // @ts-ignore ts-migrate(2554) FIXME: Expected 0 arguments, but got 2.
    {
      fields: {
        spendingPassword: {
          type: 'password',
          label: this.context.intl.formatMessage(
            globalMessages.spendingPasswordLabel
          ),
          placeholder: this.context.intl.formatMessage(
            globalMessages.spendingPasswordPlaceholder
          ),
          value: '',
          validators: [
            () => {
              // if (field.value === '') {
              //   return [
              //     false,
              //     this.context.intl.formatMessage(
              //       globalMessages.fieldIsRequired
              //     ),
              //   ];
              // }
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
      onSuccess: async (form) => {
        const { spendingPassword } = form.values();
        const formData = {
          exportType: this.state.exportType,
          password: spendingPassword || null,
        };
        await this.props.onSubmit(formData);
      },
    });
  };
  handleSubmitOnEnter = submitOnEnter.bind(this, this.submit);

  render() {
    // const { form } = this;
    const { intl } = this.context;
    const { onClose, walletName, isSubmitting, error } = this.props;
    // const { exportType } = this.state;
    const dialogClasses = classnames([styles.component, 'WalletExportDialog']);
    const actions = [
      {
        className: isSubmitting ? styles.isSubmitting : null,
        label: intl.formatMessage(messages.exportButtonLabel),
        primary: true,
        onClick: this.submit,
      },
    ];
    // const spendingPasswordField = form.$('spendingPassword');
    return (
      <Dialog
        className={dialogClasses}
        title={intl.formatMessage(messages.headline)}
        actions={actions}
        closeOnOverlayClick
        onClose={onClose}
        closeButton={<DialogCloseButton />}
      >
        {/* TODO: re-enable when we have full/readOnly exports
        <div className={styles.choices}>
         <button
           className={exportType === 'full' ? styles.activeButton : ''}
           onClick={() => this.onChangeExportType('full')}
         >
           {intl.formatMessage(messages.fullTabTitle)}
         </button>
         <button
           disabled
           className={exportType === 'readOnly' ? styles.activeButton : ''}
           onClick={() => this.onChangeExportType('readOnly')}
         >
           {intl.formatMessage(messages.readOnlyTabTitle)}
         </button>
        </div>
        */}

        <div className={styles.introduction}>
          <FormattedHTMLMessage
            {...messages.introduction}
            values={{
              walletName,
            }}
          />
        </div>

        {/*
         <Input
           className={styles.spendingPassword}
           {...spendingPasswordField.bind()}
           error={spendingPasswordField.error}
           skin={InputSkin}
           onKeyPress={this.handleSubmitOnEnter}
         />
        */}

        {error && <p className={styles.error}>{intl.formatMessage(error)}</p>}
      </Dialog>
    );
  }
}

export default ExportWalletToFileDialog;
