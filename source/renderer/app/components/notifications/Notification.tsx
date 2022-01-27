// @flow
import React, { Component, Fragment } from 'react';
import type { Node } from 'react';
import SVGInline from 'react-svg-inline';
import classNames from 'classnames';
import styles from './Notification.scss';
import closeCross from '../../assets/images/close-cross.inline.svg';
import NotificationActions from './NotificationActions';
import type { NotificationActionItems } from './NotificationActions';

export type NotificationDataProps = {
  icon?: string,
  clickToClose?: boolean,
  hasCloseButton?: boolean,
  hasEllipsis?: boolean,
  themeOverride?: 'grey', // if left empty, the notification will have its normal colors
  labelValues?: Object,
  hasSpinner?: boolean,
  actions?: NotificationActionItems,
};

type Props = {
  ...$Exact<NotificationDataProps>,
  children?: Node,
  onClose?: Function,
  isVisible: boolean,
  index?: number,
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
      actions,
      children,
      clickToClose,
      hasCloseButton,
      hasEllipsis,
      hasSpinner,
      icon,
      index,
      isVisible,
      onClose,
      themeOverride,
    } = this.props;

    const isClickToClose = clickToClose && !actions;

    const notificationMessageStyles = classNames([
      styles.component,
      isVisible ? styles.isVisible : null,
      isClickToClose ? styles.clickToClose : null,
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
        onClick={() => isClickToClose && onClose && onClose()}
        role="link"
        aria-hidden
        style={{
          zIndex: 9999999 + (index || 0),
        }}
      >
        {isVisible && (
          <Fragment>
            {icon && <SVGInline svg={icon} className={iconStyles} />}

            <div className={messageStyles}>{children}</div>

            {!!actions && <NotificationActions actions={actions} />}

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
