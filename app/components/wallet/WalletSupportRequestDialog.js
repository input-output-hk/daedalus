// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { isEmail, isEmpty } from 'validator';
import classnames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import Input from 'react-polymorph/lib/components/Input';
import SimpleInputSkin from 'react-polymorph/lib/skins/simple/InputSkin';
import TextArea from 'react-polymorph/lib/components/TextArea';
import SimpleTextAreaSkin from 'react-polymorph/lib/skins/simple/TextAreaSkin';
import Checkbox from 'react-polymorph/lib/components/Checkbox';
import SimpleSwitchSkin from 'react-polymorph/lib/skins/simple/SwitchSkin';
import ReactToolboxMobxForm from '../../utils/ReactToolboxMobxForm';
import DialogCloseButton from '../widgets/DialogCloseButton';
import Dialog from '../widgets/Dialog';
import { InvalidEmailError, FieldRequiredError } from '../../i18n/errors';
import LocalizableError from '../../i18n/LocalizableError';
import styles from './WalletSupportRequestDialog.scss';

const messages = defineMessages({
  title: {
    id: 'wallet.supportRequest.dialog.title',
    defaultMessage: '!!!Support request',
    description: 'Title for the "Settings support" dialog.',
  },
  emailLabel: {
    id: 'wallet.supportRequest.dialog.emailLabel',
    defaultMessage: '!!!Your e-mail',
    description: 'Label for the "Email" input on the wallet settings support dialog.',
  },
  emailPlaceholder: {
    id: 'wallet.supportRequest.dialog.emailPlaceholder',
    defaultMessage: '!!!Enter your e-mail here, so we can answer you',
    description: 'Placeholder for the "Email" input on the wallet settings support dialog.',
  },
  subjectLabel: {
    id: 'wallet.supportRequest.dialog.subjectLabel',
    defaultMessage: '!!!Subject',
    description: 'Label for the "Subject" input on the wallet settings support dialog.',
  },
  subjectPlaceholder: {
    id: 'wallet.supportRequest.dialog.subjectPlaceholder',
    defaultMessage: '!!!Enter subject of your problem',
    description: 'Placeholder for the "Subject" input on the wallet settings support dialog.',
  },
  problemLabel: {
    id: 'wallet.supportRequest.dialog.problemLabel',
    defaultMessage: '!!!Problem',
    description: 'Label for the "Problem" text area on the wallet settings support dialog.',
  },
  problemPlaceholder: {
    id: 'wallet.supportRequest.dialog.problemPlaceholder',
    defaultMessage: '!!!Describe steps which got you to problem',
    description: 'Placeholder for the "Problem" text area on the wallet settings support dialog.',
  },
  logsSwitchLabel: {
    id: 'wallet.supportRequest.dialog.logsSwitchLabel',
    defaultMessage: '!!!Attach logs',
    description: 'Label for the "Attach logs" switch on the wallet settings support dialog.',
  },
  logsSwitchPlaceholder: {
    id: 'wallet.supportRequest.dialog.logsSwitchPlaceholder',
    defaultMessage: '!!!Logs will help to find out problem you are describing',
    description: 'Text for the "Attach logs" switch on the wallet settings support dialog.',
  },
  submitButtonLabel: {
    id: 'wallet.supportRequest.dialog.button.label',
    defaultMessage: '!!!Send request',
    description: 'Label for the "Send request" button on the wallet settings support dialog.'
  },
});

type Props = {
  onCancel: Function,
  onSave: Function,
  isSubmitting: boolean,
  error: ?LocalizableError,
};

type State = {
  attachLogs: boolean,
};

@observer
export default class WalletSupportRequestDialog extends Component<Props, State> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    attachLogs: false,
  };

  form = new ReactToolboxMobxForm({
    fields: {
      email: {
        label: this.context.intl.formatMessage(messages.emailLabel),
        placeholder: this.context.intl.formatMessage(messages.emailPlaceholder),
        value: '',
        validators: [({ field }) => {
          const email = field.value;
          return [
            isEmail(email),
            this.context.intl.formatMessage(new InvalidEmailError())
          ];
        }]
      },
      subject: {
        label: this.context.intl.formatMessage(messages.subjectLabel),
        placeholder: this.context.intl.formatMessage(messages.subjectPlaceholder),
        value: '',
        validators: [({ field }) => {
          const subject = field.value;
          return [
            !isEmpty(subject),
            this.context.intl.formatMessage(new FieldRequiredError())
          ];
        }]
      },
      problem: {
        label: this.context.intl.formatMessage(messages.problemLabel),
        placeholder: this.context.intl.formatMessage(messages.problemPlaceholder),
        value: '',
        validators: [({ field }) => {
          const problem = field.value;
          return [
            !isEmpty(problem),
            this.context.intl.formatMessage(new FieldRequiredError())
          ];
        }]
      },
    },
  }, {
    options: {
      validateOnChange: true,
      validationDebounceWait: 250,
    },
  });

  submit = () => {
    this.form.submit({
      onSuccess: (form) => {
        const { attachLogs } = this.state;
        const { email, subject, problem } = form.values();
        const data = {
          email, subject, problem, attachLogs,
        };
        this.props.onSave(data);
      },
      onError: () => {},
    });
  };

  handleLogsSwitchToggle = (value: boolean) => {
    this.setState({ attachLogs: value });
  };

  render() {
    const { intl } = this.context;
    const { form } = this;
    const { onCancel, isSubmitting, error } = this.props;
    const { attachLogs } = this.state;

    const attachedLogsClasses = classnames([
      styles.attachedLogs,
      attachLogs ? styles.show : null,
    ]);

    const submitButtonClasses = classnames([
      'submitButton',
      isSubmitting ? styles.isSubmitting : null,
    ]);

    const emailField = form.$('email');
    const subjectField = form.$('subject');
    const problemField = form.$('problem');

    const actions = [
      {
        className: submitButtonClasses,
        label: this.context.intl.formatMessage(messages.submitButtonLabel),
        primary: true,
        disabled: isSubmitting,
        onClick: this.submit,
      },
    ];

    return (
      <Dialog
        className="supportRequestDialog"
        title={intl.formatMessage(messages.title)}
        actions={actions}
        closeOnOverlayClick
        onClose={onCancel}
        closeButton={<DialogCloseButton onClose={onCancel} />}
      >

        <div className={styles.emailInput}>
          <Input
            className="email"
            {...emailField.bind()}
            error={emailField.error}
            skin={<SimpleInputSkin />}
          />
        </div>

        <div className={styles.subjectInput}>
          <Input
            className="subject"
            {...subjectField.bind()}
            error={subjectField.error}
            skin={<SimpleInputSkin />}
          />
        </div>

        <div className={styles.problemTextarea}>
          <TextArea
            className="problemDescription"
            autoResize={false}
            rows={3}
            {...problemField.bind()}
            error={problemField.error}
            skin={<SimpleTextAreaSkin />}
          />
        </div>

        <div className={styles.attachLogs}>
          <div className={styles.logsSwitch}>
            <div className={styles.logsSwitchlabel}>
              {intl.formatMessage(messages.logsSwitchLabel)}
            </div>

            <Checkbox
              onChange={this.handleLogsSwitchToggle}
              label={intl.formatMessage(messages.logsSwitchPlaceholder)}
              checked={attachLogs}
              skin={<SimpleSwitchSkin />}
            />
          </div>

          <div className={attachedLogsClasses}>
            {/* TODO - get logs from disc */}
            <p>/Users/darkomijic/Library/Application Support/Daedalus/Logs/Daedalus.log</p>
            <p>/Users/darkomijic/Library/Application Support/Daedalus/Logs/Daedalus.log</p>
          </div>
        </div>

        {error ? <p className={styles.error}>{intl.formatMessage(error)}</p> : null}

      </Dialog>
    );
  }

}
