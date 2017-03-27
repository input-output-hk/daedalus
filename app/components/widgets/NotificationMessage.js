import React, { Component, PropTypes } from 'react';
import classNames from 'classnames';
import { oneOrManyChildElements } from '../../propTypes';
import styles from './NotificationMessage.scss';

export default class NotificationMessage extends Component {

  static propTypes = {
    icon: PropTypes.string,
    show: PropTypes.bool.isRequired,
    children: oneOrManyChildElements.isRequired,
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
