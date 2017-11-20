// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import Button from 'react-polymorph/lib/components/Button';
import SimpleButtonSkin from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { defineMessages, intlShape } from 'react-intl';
import LocalizableError from '../../../i18n/LocalizableError';
import styles from './SendLogsChoiceForm.scss';

const messages = defineMessages({
  continueButtonLabel: {
    id: 'profile.sendLogsChoice.form.continueButtonLabel',
    defaultMessage: '!!!Continue',
    description: 'Label for the "Send logs" accept button',
  },
  declineButtonLabel: {
    id: 'profile.sendLogsChoice.form.declineButtonLabel',
    defaultMessage: '!!!Dont\'t send',
    description: 'Label for the "Send logs" decline button',
  },
  contentLabel: {
    id: 'profile.sendLogsChoice.form.contentLabel',
    defaultMessage: '!!!Send logs to the central server',
    description: 'Label for the "Send logs" content',
  },
  contentText: {
    id: 'profile.sendLogsChoice.form.contentText',
    defaultMessage: '!!!Do you want to help diagnose issues by opting in to send logs to our central logging server? Logs will not include any sensitive data.',
    description: 'Send logs content',
  },
});

type Props = {
  onSubmit: Function,
  isSubmitting: boolean,
  error?: ?LocalizableError,
};

@observer
export default class SendLogsChoiceForm extends Component<Props> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  submit = (value: boolean) => {
    this.props.onSubmit({ sendLogs: value });
  };

  render() {
    const { intl } = this.context;
    const { isSubmitting, error } = this.props;

    const acceptButtonClasses = classnames([
      'primary',
      'acceptButton',
      styles.button,
      isSubmitting ? styles.submitButtonSpinning : null,
    ]);

    const declineButtonClasses = classnames([
      'flat',
      styles.button,
      styles.declineButton,
      isSubmitting ? styles.submitButtonSpinning : null,
    ]);

    return (
      <div className={styles.component}>
        <div className={styles.centeredBox}>

          <p className={styles.sendLogsLabel}>{intl.formatMessage(messages.contentLabel)}</p>

          <p className={styles.sendLogsText}>{intl.formatMessage(messages.contentText)}</p>

          {error && <p className={styles.error}>{intl.formatMessage(error)}</p>}

          <div className={styles.actions}>

            <Button
              className={declineButtonClasses}
              label={intl.formatMessage(messages.declineButtonLabel)}
              onMouseUp={this.submit.bind(this, false)}
              skin={<SimpleButtonSkin />}
            />

            <Button
              className={acceptButtonClasses}
              label={intl.formatMessage(messages.continueButtonLabel)}
              onMouseUp={this.submit.bind(this, true)}
              skin={<SimpleButtonSkin />}
            />

          </div>

        </div>
      </div>
    );
  }

}
