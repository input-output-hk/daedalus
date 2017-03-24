import React, { Component, PropTypes } from 'react';
import classNames from 'classnames';
import styles from './NotificationMessage.scss';

export default class NotificationMessage extends Component {

  static propTypes = {
    icon: PropTypes.string,
    message: PropTypes.string.isRequired,
    show: PropTypes.bool,
  };

  render() {
    const { icon, message, show } = this.props;

    const notificationMessageStyles = classNames([
      styles.component,
      show ? styles.show : null,
    ]);

    return (
      <div className={notificationMessageStyles}>

        {icon && <img className={styles.icon} src={icon} role="presentation" />}

        <span className={styles.message} dangerouslySetInnerHTML={{ __html: message }} />

      </div>
    );
  }

}
