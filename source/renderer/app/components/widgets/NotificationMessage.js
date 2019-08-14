// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import SVGInline from 'react-svg-inline';
import classNames from 'classnames';
import styles from './NotificationMessage.scss';
import closeCross from '../../assets/images/close-cross.inline.svg';

export type Props = {
  icon?: string,
  iconStyle?: Object,
  show: boolean,
  children?: Node,
  clickToClose?: boolean,
  hasCloseButton?: boolean,
  onClose?: Function,
  order?: 'auto' | number | 'initial' | 'inherit',
  themeOverride?: 'grey', // if left empty, the noticiation will have its normal colors
};

export default class NotificationMessage extends Component<Props> {
  static defaultProps = {
    order: 'auto',
  };

  render() {
    const {
      icon,
      iconStyle,
      show,
      children,
      clickToClose,
      hasCloseButton,
      onClose,
      order,
      themeOverride,
    } = this.props;

    const notificationMessageStyles = classNames([
      styles.component,
      show ? styles.show : null,
      clickToClose ? styles.clickToClose : null,
      themeOverride ? styles[`theme-override-${themeOverride}`] : null,
    ]);

    const iconStyles = classNames([styles.icon, iconStyle]);

    return (
      <div
        className={notificationMessageStyles}
        onClick={() => clickToClose && onClose && onClose()}
        role="link"
        aria-hidden
        style={{
          zIndex: order,
        }}
      >
        {icon && <SVGInline svg={icon} className={iconStyles} />}

        <div className={styles.message}>{children}</div>

        {hasCloseButton && (
          <button
            className={styles.closeButton}
            onClick={() => onClose && onClose()}
          >
            <SVGInline svg={closeCross} />
          </button>
        )}
      </div>
    );
  }
}
