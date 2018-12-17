// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import SVGInline from 'react-svg-inline';
import classNames from 'classnames';
import styles from './NotificationMessage.scss';
import closeCross from '../../assets/images/close-cross.inline.svg';

type Props = {
  icon: string,
  show: boolean,
  children?: Node,
  clickToClose?: boolean,
  hasCloseButton?: boolean,
  onClose?: Function,
};

export default class NotificationMessage extends Component<Props> {

  render() {
    const { icon, show, children, clickToClose, hasCloseButton, onClose } = this.props;

    const notificationMessageStyles = classNames([
      styles.component,
      show ? styles.show : null,
      clickToClose ? styles.clickToClose : null,
    ]);

    return (
      <div
        className={notificationMessageStyles}
        onClick={() => clickToClose && onClose && onClose()}
        role="link"
        aria-hidden
      >

        {icon && <SVGInline svg={icon} className={styles.icon} />}

        <div className={styles.message}>
          {children}
        </div>

        {
          hasCloseButton &&
          <button className={styles.closeButton} onClick={() => onClose && onClose()}>
            <SVGInline svg={closeCross} />
          </button>
        }

      </div>
    );
  }

}
