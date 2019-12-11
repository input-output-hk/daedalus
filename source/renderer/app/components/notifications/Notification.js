// @flow
import React, { Component, Fragment } from 'react';
import type { Node } from 'react';
import SVGInline from 'react-svg-inline';
// import { CSSTransition } from 'react-transition-group';
import classNames from 'classnames';
import styles from './Notification.scss';
// import notificationTransitionsStyles from './NotificationTransitions.scss';
import closeCross from '../../assets/images/close-cross.inline.svg';

export type NotificationMessageProps = {
  icon?: string,
  clickToClose?: boolean,
  hasCloseButton?: boolean,
  hasEllipsis?: boolean,
  themeOverride?: 'grey', // if left empty, the noticiation will have its normal colors
  labelValues?: Object,
};

type Props = {
  ...$Exact<NotificationMessageProps>,
  label?: Node,
  onClose?: Function,
  order?: 'auto' | number | 'initial' | 'inherit',
};

export default class Notification extends Component<Props> {
  static defaultProps = {
    order: 9999999999999999,
    clickToClose: true,
    hasCloseButton: true,
    hasEllipsis: true,
  };

  render() {
    const {
      icon,
      label,
      clickToClose,
      hasCloseButton,
      hasEllipsis,
      onClose,
      order,
      themeOverride,
    } = this.props;

    const notificationMessageStyles = classNames([
      styles.component,
      hasEllipsis ? styles.hasEllipsis : null,
      clickToClose ? styles.clickToClose : null,
      themeOverride ? styles[`theme-override-${themeOverride}`] : null,
    ]);

    // const transitionsStyles = classNames([
    //   styles.transition,
    //   notificationTransitionsStyles,
    // ]);

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
        <Fragment>
          {icon && <SVGInline svg={icon} className={styles.icon} />}

          <div className={styles.message}>{label}</div>

          {hasCloseButton && (
            <button
              className={styles.closeButton}
              onClick={() => onClose && onClose()}
            >
              <SVGInline svg={closeCross} />
            </button>
          )}
        </Fragment>
      </div>
    );
  }
}
