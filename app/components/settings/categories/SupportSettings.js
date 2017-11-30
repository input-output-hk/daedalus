// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import Checkbox from 'react-polymorph/lib/components/Checkbox';
import SimpleSwitchSkin from 'react-polymorph/lib/skins/simple/SwitchSkin';
import LocalizableError from '../../../i18n/LocalizableError';
import styles from './SupportSettings.scss';

const messages = defineMessages({
  logsSwitchLabel: {
    id: 'settings.support.sendLogs.switchLabel',
    defaultMessage: '!!!Send logs to the central server',
    description: 'Label for the "Send logs" switch on the support settings page.',
  },
  logsSwitchPlaceholder: {
    id: 'settings.support.sendLogs.switchPlaceholder',
    defaultMessage: '!!!Do you want to help diagnose issues by opting in to send logs to our central logging server? Logs will not include any sensitive data.',
    description: 'Text for the "Send logs" switch on the support settings page.',
  },
});

type Props = {
  onSubmit: Function,
  error?: ?LocalizableError,
  sendLogs: boolean,
};

@observer
export default class SupportSettings extends Component<Props> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  handleLogsSwitchToggle = (value: boolean) => {
    this.props.onSubmit({ sendLogs: value });
  };

  render() {
    const { error, sendLogs } = this.props;
    const { intl } = this.context;

    return (
      <div className={styles.component}>

        <div className={styles.label}>
          {intl.formatMessage(messages.logsSwitchLabel)}
        </div>

        <Checkbox
          onChange={this.handleLogsSwitchToggle}
          label={intl.formatMessage(messages.logsSwitchPlaceholder)}
          checked={sendLogs}
          skin={<SimpleSwitchSkin />}
        />

        {error && <p className={styles.error}>{error}</p>}

      </div>
    );
  }

}
