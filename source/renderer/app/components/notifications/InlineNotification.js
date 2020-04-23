// @flow
import React, { Component } from 'react';
import classNames from 'classnames';
import styles from './InlineNotification.scss';

type Props = {
  show: boolean,
  children?: string,
};

export default class InlineNotification extends Component<Props> {
  render() {
    const { show, children } = this.props;

    const notificationMessageStyles = classNames([
      styles.component,
      show ? styles.show : null,
    ]);

    return <div className={notificationMessageStyles}>{children}</div>;
  }
}
