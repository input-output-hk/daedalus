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
    defaultMessage: '!!!Wallet restore in progress',
    description: 'Status message "Wallet restore in progress" shown while wallet is being restored.'
  },
});

type Props = {
  isRestoreActive: boolean,
};

@observer
export default class RestoreNotification extends Component<Props> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { isRestoreActive } = this.props;

    const restoreMessageClasses = classnames([
      styles.message,
      'ActiveRestoreNotification',
    ]);

    return (
      <div className={styles.component}>
        {isRestoreActive && (
          <div className={restoreMessageClasses}>
            <span className={styles.text}>{intl.formatMessage(messages.activeRestoreMessage)}</span>
            <SVGInline svg={spinnerIcon} className={styles.icon} />
          </div>
        )}
      </div>
    );
  }
}
