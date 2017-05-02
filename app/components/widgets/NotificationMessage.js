import React, { Component } from 'react';
import type { Children } from 'react';
import classNames from 'classnames';
import styles from './NotificationMessage.scss';

export default class NotificationMessage extends Component {

  props: {
    notificationNumber: number,
    icon: string,
    show: boolean,
    children?: Children,
  };

  render() {
    const { icon, show, children, notificationNumber } = this.props;
    const topPosition = (notificationNumber * 62 + notificationNumber) + 'px';

    const notificationMessageStyles = classNames([
      styles.component,
      show ? styles.show : null,
    ]);

    return (
      <div className={notificationMessageStyles} style={{ top: topPosition }}>

        {icon && <img className={styles.icon} src={icon} role="presentation" />}

        <div className={styles.message}>
          {children}
        </div>

      </div>
    );
  }

}
