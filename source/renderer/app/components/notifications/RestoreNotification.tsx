import React, { Component } from 'react';
import classnames from 'classnames';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import { defineMessages, intlShape } from 'react-intl';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../assets/images/spinner-da... Remove this comment to see the full error message
import spinnerIcon from '../../assets/images/spinner-dark.inline.svg';
import { formattedNumber } from '../../utils/formatters';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './RestoreNotification.scss' or... Remove this comment to see the full error message
import styles from './RestoreNotification.scss';

const messages = defineMessages({
  activeRestoreMessage: {
    id: 'wallet.statusMessages.activeRestore',
    defaultMessage:
      '!!!The balance and transaction history of this wallet is {percentage}% synced with the blockchain.',
    description:
      'Status message "Wallet restore in progress" shown while wallet is being restored.',
  },
});
type Props = {
  restoreProgress: number;
};

@observer
class RestoreNotification extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { restoreProgress } = this.props;
    const restoreNotificationClasses = classnames([
      styles.component,
      'ActiveRestoreNotification',
    ]);
    return (
      <div className={restoreNotificationClasses}>
        <span className={styles.text}>
          {intl.formatMessage(messages.activeRestoreMessage, {
            percentage: formattedNumber(restoreProgress),
          })}
        </span>
        <SVGInline svg={spinnerIcon} className={styles.icon} />
      </div>
    );
  }
}

export default RestoreNotification;
