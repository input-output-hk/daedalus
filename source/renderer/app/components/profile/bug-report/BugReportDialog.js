// @flow
import React, { Component } from 'react';
import { map, get } from 'lodash';
import { observer } from 'mobx-react';
import { isEmail, isEmpty } from 'validator';
import classnames from 'classnames';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import Input from 'react-polymorph/lib/components/Input';
import SimpleInputSkin from 'react-polymorph/lib/skins/simple/raw/InputSkin';
import TextArea from 'react-polymorph/lib/components/TextArea';
import SimpleTextAreaSkin from 'react-polymorph/lib/skins/simple/raw/TextAreaSkin';
import Checkbox from 'react-polymorph/lib/components/Checkbox';
import SimpleSwitchSkin from 'react-polymorph/lib/skins/simple/raw/SwitchSkin';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import { InvalidEmailError, FieldRequiredError } from '../../../i18n/errors';
import LocalizableError from '../../../i18n/LocalizableError';
import styles from './BugReportDialog.scss';
import type { LogFiles } from '../../../types/LogTypes';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';

const messages = defineMessages({
  title: {
    id: 'bugReport.dialog.title',
    defaultMessage: '!!!Support request',
    description: 'Title for the "Settings support" dialog.',
  },
  emailLabel: {
    id: 'bugReport.dialog.emailLabel',
    defaultMessage: '!!!Your e-mail',
    description: 'Label for the "Email" input on the wallet settings support dialog.',
  },
  emailPlaceholder: {
    id: 'bugReport.dialog.emailPlaceholder',
    defaultMessage: '!!!Enter your e-mail here, so we can answer you',
    description: 'Placeholder for the "Email" input on the wallet settings support dialog.',
  },
  subjectLabel: {
    id: 'bugReport.dialog.subjectLabel',
    defaultMessage: '!!!Subject',
    description: 'Label for the "Subject" input on the wallet settings support dialog.',
  },
  subjectPlaceholder: {
    id: 'bugReport.dialog.subjectPlaceholder',
    defaultMessage: '!!!Enter subject of your problem',
    description: 'Placeholder for the "Subject" input on the wallet settings support dialog.',
  },
  problemLabel: {
    id: 'bugReport.dialog.problemLabel',
    defaultMessage: '!!!Problem',
    description: 'Label for the "Problem" text area on the wallet settings support dialog.',
  },
  problemPlaceholder: {
    id: 'bugReport.dialog.problemPlaceholder',
    defaultMessage: '!!!Describe steps which got you to problem',
    description: 'Placeholder for the "Problem" text area on the wallet settings support dialog.',
  },
  logsSwitchLabel: {
    id: 'bugReport.dialog.logsSwitchLabel',
    defaultMessage: '!!!Attach logs',
    description: 'Label for the "Attach logs" switch on the wallet settings support dialog.',
  },
  logsSwitchPlaceholder: {
    id: 'bugReport.dialog.logsSwitchPlaceholder',
    defaultMessage: '!!!Logs will help to find out problem you are describing',
    description: 'Text for the "Attach logs" switch on the wallet settings support dialog.',
  },
  submitButtonLabel: {
    id: 'bugReport.dialog.button.label',
    defaultMessage: '!!!Send request',
    description: 'Label for the "Send request" button on the wallet settings support dialog.'
  },
  alternativeDescription: {
    id: 'bugReport.dialog.alternative.description',
    defaultMessage: `!!!Alternatively, to help the development team investigate the issue you are experiencing,
    you can send your support request manually. You should first download your logs.
    Please take the following steps to submit your support request:`,
    description: 'Bug report dialog alternative description text.'
  },
  alternativeErrorMessage: {
    id: 'bugReport.dialog.alternative.errorMessage',
    defaultMessage: '!!!There was a problem sending the support request.',
    description: 'Bug report dialog alternative error message.'
  },
  alternativeInstructionsStep1: {
    id: 'bugReport.dialog.alternative.instructions.step1',
    defaultMessage: '!!!Click the Download logs button to retrieve your archived logs, and save the file on your desktop.',
    description: 'Bug report dialog alternative instructions step one.'
  },
  alternativeInstructionsStep2: {
    id: 'bugReport.dialog.alternative.instructions.step2',
    defaultMessage: '!!!Click the Submit manually button, which will take you to the issue-reporting page on the Daedalus website.',
    description: 'Bug report dialog alternative instructions step two.'
  },
  alternativeInstructionsStep3: {
    id: 'bugReport.dialog.alternative.instructions.step3',
    defaultMessage: '!!!Attach the logs to your support request, fill in your details, and submit the form.',
    description: 'Bug report dialog alternative instructions step three.'
  },
  submitManuallyButtonLabel: {
    id: 'bugReport.dialog.alternative.submitManually.button.label',
    defaultMessage: '!!!Submit manually',
    description: 'Label for the "Submit manually" button on the wallet settings support dialog.'
  },
  submitManuallyLink: {
    id: 'bugReport.dialog.alternative.submitManually.link',
    defaultMessage: '!!!daedaluswallet.io/problem',
    description: 'Link to Daedalus website "Problem" page'
  },
  downloadButtonLabel: {
    id: 'bugReport.dialog.alternative.download.button.label',
    defaultMessage: '!!!Download',
    description: 'Label for the "Download" button on the wallet settings support dialog.'
  },
});

type Props = {
  logFiles: LogFiles,
  compressedLog: ?string,
  onCancel: Function,
  onSubmit: Function,
  onSubmitManually: Function,
  onDownload: Function,
  onGetLogs: Function,
  onCompressLogs: Function,
  onDeleteCompressedLogs: Function,
  isSubmitting: boolean,
  isCompressing: boolean,
  isDownloading?: boolean,
  error: ?LocalizableError,
};

type State = {
  showLogs: boolean,
};

@observer
export default class BugReportDialog extends Component<Props, State> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    showLogs: true,
  };

  componentWillMount() {
    this.props.onGetLogs();
    this.props.onDeleteCompressedLogs();
  }

  componentWillReceiveProps(nextProps: Object) {
    const commpressionFilesChanged = this.props.compressedLog !== nextProps.compressedLog;
    if (nextProps.compressedLog && commpressionFilesChanged && !nextProps.isDownloading) {
      // proceed to submit when ipc rendered successfully return compressed files
      this.submit(nextProps.compressedLog);
    }
  }

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
      validationDebounceWait: FORM_VALIDATION_DEBOUNCE_WAIT,
    },
  });

  submit = (compressedLog: ?string) => {
    this.form.submit({
      onSuccess: (form) => {
        const { logFiles } = this.props;
        const logsExist = get(logFiles, ['files'], []).length > 0;

        const { email, subject, problem } = form.values();
        const data = {
          email, subject, problem, compressedLog
        };

        if (this.state.showLogs && logsExist && !compressedLog) {
          // submit request with commpressed logs files
          this.props.onCompressLogs(this.props.logFiles);
        } else {
          // regular submit
          this.props.onSubmit(data);
        }
      },
      onError: () => {},
    });
  };

  handleLogsSwitchToggle = (value: boolean) => {
    this.setState({ showLogs: value });
  };

  render() {
    const { intl } = this.context;
    const { showLogs } = this.state;
    const { form } = this;
    const {
      onCancel, isSubmitting, isCompressing,
      logFiles, error, onDownload, isDownloading,
    } = this.props;

    const submitManuallyLink = intl.formatMessage(messages.submitManuallyLink);

    const logsExist = get(logFiles, ['files'], []).length > 0;
    const logsPath = get(logFiles, 'path');
    const fileNames = get(logFiles, 'files');

    const attachedLogsClasses = classnames([
      styles.attachedLogs,
      (showLogs && logFiles) ? styles.show : null,
    ]);

    const submitButtonClasses = classnames([
      'submitButton',
      (isSubmitting || isCompressing) ? styles.isSubmitting : null,
    ]);

    const downloadButtonClasses = classnames([
      'downloadButton',
      isDownloading ? styles.isSubmitting : null,
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
        onClick: this.submit.bind(this, null),
      },
    ];

    const alternativeActions = [
      {
        className: downloadButtonClasses,
        label: this.context.intl.formatMessage(messages.downloadButtonLabel),
        primary: true,
        disabled: isDownloading,
        onClick: onDownload.bind(this),
      },
      {
        className: 'submitManuallyButton',
        label: this.context.intl.formatMessage(messages.submitManuallyButtonLabel),
        primary: true,
        onClick: this.onSubmitManually.bind(this, submitManuallyLink),
      },
    ];

    return (
      <Dialog
        className={styles.dialog}
        title={intl.formatMessage(messages.title)}
        actions={!error ? actions : alternativeActions}
        closeOnOverlayClick
        onClose={onCancel}
        closeButton={<DialogCloseButton onClose={onCancel} />}
      >
        {error ? (
          <div>
            <p className={styles.error}>{intl.formatMessage(messages.alternativeErrorMessage)}</p>
            <div className={styles.bugReportAlternativeText}>
              <p><FormattedHTMLMessage {...messages.alternativeDescription} /></p>
              <ol>
                <li><FormattedHTMLMessage {...messages.alternativeInstructionsStep1} /></li>
                <li><FormattedHTMLMessage {...messages.alternativeInstructionsStep2} /></li>
                <li><FormattedHTMLMessage {...messages.alternativeInstructionsStep3} /></li>
              </ol>
            </div>
          </div>
        ) : (
          <div>
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

            <div className={styles.logsWrapper}>
              <div className={styles.logsSwitch}>
                <div className={styles.logsSwitchlabel}>
                  {intl.formatMessage(messages.logsSwitchLabel)}
                </div>

                <Checkbox
                  onChange={this.handleLogsSwitchToggle}
                  label={intl.formatMessage(messages.logsSwitchPlaceholder)}
                  checked={showLogs}
                  skin={<SimpleSwitchSkin />}
                />
              </div>

              {logsExist && (
                <div className={attachedLogsClasses}>
                  <p className={styles.logPath}>{logsPath}</p>
                  {map(fileNames, (fileName) => (
                    <p className={styles.logFileName} key={fileName}>{fileName}</p>
                  ))}
                </div>
              )}
            </div>
          </div>
        )}
      </Dialog>
    );
  }

  onSubmitManually = (link: string) => {
    this.props.onSubmitManually(link);
  };
}
