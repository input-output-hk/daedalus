import React, { Component } from 'react';
import type { Children } from 'react';
import SvgInline from 'react-svg-inline';
import classNames from 'classnames';
import styles from './NotificationMessage.scss';

type Props = {
  icon: string,
  show: boolean,
  children?: Children,
};

export default class NotificationMessage extends Component<Props> {

  render() {
    const { icon, show, children } = this.props;

    const notificationMessageStyles = classNames([
      styles.component,
      show ? styles.show : null,
    ]);

    return (
      <div className={notificationMessageStyles}>

        {icon && <SvgInline svg={icon} className={styles.icon} />}

        <div className={styles.message}>
          {children}
        </div>

      </div>
    );
  }

}
