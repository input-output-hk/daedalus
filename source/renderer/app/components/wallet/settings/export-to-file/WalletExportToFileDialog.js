// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import Input from 'react-polymorph/lib/components/Input';
import SimpleInputSkin from 'react-polymorph/lib/skins/simple/raw/InputSkin';
import DialogCloseButton from '../../../widgets/DialogCloseButton';
import ReactToolboxMobxForm from '../../../../utils/ReactToolboxMobxForm';
import globalMessages from '../../../../i18n/global-messages';
import Dialog from '../../../widgets/Dialog';
import LocalizableError from '../../../../i18n/LocalizableError';
import styles from './WalletExportToFileDialog.scss';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../../config/timingConfig';

const messages = defineMessages({
  headline: {
    id: 'wallet.exportToFile.dialog.headline',
    defaultMessage: '!!!Export Wallet',
    description: 'headline for "export wallet to file" dialog.'
  },
  introduction: {
    id: 'wallet.exportToFile.dialog.introduction',
    defaultMessage: '!!!You are exporting <strong>{walletName}</strong> to a file.',
    description: 'headline for "export wallet to file" dialog.'
  },
  exportButtonLabel: {
    id: 'wallet.exportToFile.dialog.submit.label',
    defaultMessage: '!!!Export',
    description: 'Label for export wallet to file submit button.'
  },
  // TODO: re-enable when we have full/readOnly exports
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
  exportType: ExportType,
  password: ?string,
};

type Props = {
  walletName: string,
  hasSpendingPassword: boolean,
  isSubmitting: boolean,
  onSubmit: (OnSubmitParams) => void,
  onClose: () => void,
  error?: ?LocalizableError,
};

type State = {
  exportType: ExportType,
};

@observer
export default class WalletExportToFileDialog extends Component<Props, State> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  constructor(props: any, children: any) {
    super(props, children);
    this.state = {
      exportType: EXPORT_TYPE.FULL,
    };
  }

  // onChangeExportType(exportType: ExportType) {
  //   this.setState({ exportType });
  // }

  form = new ReactToolboxMobxForm({
    fields: {
      spendingPassword: {
        type: 'password',
        label: this.context.intl.formatMessage(globalMessages.spendingPasswordLabel),
        placeholder: this.context.intl.formatMessage(globalMessages.spendingPasswordPlaceholder),
        value: '',
        validators: [({ field }) => {
          if (this.props.hasSpendingPassword && field.value === '') {
            return [false, this.context.intl.formatMessage(globalMessages.fieldIsRequired)];
          }
          return [true];
        }],
      },
    }
  }, {
    options: {
      validateOnChange: true,
      validationDebounceWait: FORM_VALIDATION_DEBOUNCE_WAIT,
    },
  });

  submit = () => {
    this.form.submit({
      onSuccess: (form) => {
        const { hasSpendingPassword } = this.props;
        const { spendingPassword } = form.values();
        const formData = {
          exportType: this.state.exportType,
          password: hasSpendingPassword ? spendingPassword : null,
        };
        this.props.onSubmit(formData);
      },
    });
  };

  render() {
    const { form } = this;
    const { intl } = this.context;
    const {
      onClose, walletName,
      hasSpendingPassword,
      isSubmitting, error,
    } = this.props;
    // const { exportType } = this.state;
    const dialogClasses = classnames([
      styles.component,
      'WalletExportDialog',
    ]);

    const actions = [
      {
        className: isSubmitting ? styles.isSubmitting : null,
        label: intl.formatMessage(messages.exportButtonLabel),
        primary: true,
        onClick: this.submit,
      }
    ];

    const spendingPasswordField = form.$('spendingPassword');

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
            values={{ walletName }}
          />
        </div>

        {hasSpendingPassword ? (
          <Input
            className={styles.spendingPassword}
            {...spendingPasswordField.bind()}
            error={spendingPasswordField.error}
            skin={<SimpleInputSkin />}
          />
        ) : null}

        {error && <p className={styles.error}>{intl.formatMessage(error)}</p>}

      </Dialog>
    );
  }

}
