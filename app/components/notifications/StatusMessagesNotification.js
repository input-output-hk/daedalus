// @flow
import React, { Component } from 'react';
import SvgInline from 'react-svg-inline';
import { defineMessages, intlShape } from 'react-intl';
import spinnerIcon from '../../assets/images/spinner-dark.inline.svg';
import styles from './StatusMessagesNotification.scss';

const messages = defineMessages({
  activeImportMessage: {
    id: 'wallet.statusMessages.activeImport',
    defaultMessage: '!!!Wallet import in progress',
    description: 'Status message "Wallet import in progress" shown while wallet is being imported.'
  },
  activeRestoreMessage: {
    id: 'wallet.statusMessages.activeRestore',
    defaultMessage: '!!!Wallet restore in progress',
    description: 'Status message "Wallet restore in progress" shown while wallet is being restored.'
  },
});

export default class StatusMessagesNotification extends Component {

  props: {
    isImportActive: boolean,
    isRestoreActive: boolean,
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { isImportActive, isRestoreActive } = this.props;
    return (
      <div className={styles.component}>
        {isImportActive && (
          <div className={styles.message}>
            <span className={styles.text}>{intl.formatMessage(messages.activeImportMessage)}</span>
            <SvgInline svg={spinnerIcon} className={styles.icon} />
          </div>
        )}

        {isRestoreActive && (
          <div className={styles.message}>
            <span className={styles.text}>{intl.formatMessage(messages.activeRestoreMessage)}</span>
            <SvgInline svg={spinnerIcon} className={styles.icon} />
          </div>
        )}
      </div>
    );
  }
}
