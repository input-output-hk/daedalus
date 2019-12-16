// @flow
import React, { Component, Fragment } from 'react';
import type { Node } from 'react';
import SVGInline from 'react-svg-inline';
import classNames from 'classnames';
import styles from './Notification.scss';
import closeCross from '../../assets/images/close-cross.inline.svg';

export type NotificationMessageProps = {
  icon?: string,
  clickToClose?: boolean,
  hasCloseButton?: boolean,
  hasEllipsis?: boolean,
  themeOverride?: 'grey', // if left empty, the noticiation will have its normal colors
  labelValues?: Object,
  hasSpinner?: boolean,
};

type Props = {
  ...$Exact<NotificationMessageProps>,
  children?: Node,
  onClose?: Function,
  isVisible: boolean,
  index: number,
};

export default class Notification extends Component<Props> {
  static defaultProps = {
    clickToClose: true,
    hasCloseButton: true,
    hasEllipsis: false,
    hasSpinner: false,
  };

  render() {
    const {
      icon,
      children,
      clickToClose,
      hasCloseButton,
      hasEllipsis,
      onClose,
      index,
      themeOverride,
      isVisible,
      hasSpinner,
    } = this.props;

    const notificationMessageStyles = classNames([
      styles.component,
      isVisible ? styles.isVisible : null,
      clickToClose ? styles.clickToClose : null,
      themeOverride ? styles[`theme-override-${themeOverride}`] : null,
    ]);

    const messageStyles = classNames([
      styles.message,
      hasEllipsis ? styles.hasEllipsis : null,
    ]);

    const iconStyles = classNames([
      styles.icon,
      hasSpinner ? styles.spinnerIcon : null,
    ]);

    return (
      <div
        className={notificationMessageStyles}
        onClick={() => clickToClose && onClose && onClose()}
        role="link"
        aria-hidden
        style={{
          zIndex: 9999999 + index,
        }}
      >
        {isVisible && (
          <Fragment>
            {icon && <SVGInline svg={icon} className={iconStyles} />}

            <div className={messageStyles}>{children}</div>

            {hasCloseButton && (
              <button
                className={styles.closeButton}
                onClick={() => onClose && onClose()}
              >
                <SVGInline svg={closeCross} />
              </button>
            )}
          </Fragment>
        )}
      </div>
    );
  }
}
