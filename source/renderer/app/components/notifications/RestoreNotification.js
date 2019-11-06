// @flow
import React, { Component } from 'react';
import classnames from 'classnames';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import { defineMessages, intlShape } from 'react-intl';
import spinnerIcon from '../../assets/images/spinner-dark.inline.svg';
import styles from './RestoreNotification.scss';

const messages = defineMessages({
  activeRestoreMessage: {
    id: 'wallet.statusMessages.activeRestore',
    defaultMessage:
      '!!!The balance and transaction history of this wallet is being synced with the blockchain.',
    description:
      'Status message "Wallet restore in progress" shown while wallet is being restored.',
  },
});

type Props = {
  restoreProgress?: number,
};

@observer
export default class RestoreNotification extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;

    const restoreNotificationClasses = classnames([
      styles.component,
      'ActiveRestoreNotification',
    ]);

    return (
      <div className={restoreNotificationClasses}>
        <span className={styles.text}>
          {intl.formatMessage(messages.activeRestoreMessage)}
        </span>
        <SVGInline svg={spinnerIcon} className={styles.icon} />
      </div>
    );
  }
}
