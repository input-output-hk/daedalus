// @flow
import React, { Component } from 'react';
import classnames from 'classnames';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import styles from './AntivirusRestaurationSlowdownNotification.scss';
import closeCrossWhite from '../../assets/images/close-cross-white.inline.svg';

const messages = defineMessages({
  notification: {
    id: 'wallet.statusMessages.antivirusRestaurationSlowdownNotification',
    defaultMessage: '!!!<strong>Note:</strong> Antivirus software, like Windows Defender, can cause slow wallet restoration times.',
    description: 'Warning about antivirus software slowing down restoration process on Windows.'
  },
});

@observer
export default class AntivirusRestaurationSlowdownNotification extends Component<any> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const notificationClasses = classnames([
      styles.component,
      'AntivirusRestaurationSlowdownNotification',
    ]);

    return (
      <div className={notificationClasses}>
        <div className={styles.text}>
          <FormattedHTMLMessage {...messages.notification} />
        </div>
        <div className={styles.closeButton}>
          <SVGInline className={styles.closeCross} svg={closeCrossWhite} />
        </div>
      </div>
    );
  }
}
