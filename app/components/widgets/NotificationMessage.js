import React, { Component } from 'react';
import type { Children } from 'react';
import classNames from 'classnames';
import styles from './NotificationMessage.scss';

export default class NotificationMessage extends Component {

  props: {
    icon: string,
    show: boolean,
    children?: Children,
  };

  render() {
    const { icon, show, children } = this.props;

    const notificationMessageStyles = classNames([
      styles.component,
      show ? styles.show : null,
    ]);

    return (
      <div className={notificationMessageStyles}>

        {icon && <img className={styles.icon} src={icon} role="presentation" />}

        <div className={styles.message}>
          {children}
        </div>

      </div>
    );
  }

}
