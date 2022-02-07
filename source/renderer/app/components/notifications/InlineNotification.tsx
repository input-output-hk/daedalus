import React, { Component } from 'react';
import classNames from 'classnames';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './InlineNotification.scss' or ... Remove this comment to see the full error message
import styles from './InlineNotification.scss';

type Props = {
  show: boolean;
  children?: string;
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
