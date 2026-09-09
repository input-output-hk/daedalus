import React, { Component, Fragment } from 'react';
// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'Node'.
import type { Node } from 'react';
import SVGInline from 'react-svg-inline';
import classNames from 'classnames';
import { intlShape } from 'react-intl';
import globalMessages from '../../i18n/global-messages';
import styles from './Notification.scss';
import closeCross from '../../assets/images/close-cross.inline.svg';
import NotificationActions from './NotificationActions';
import type { NotificationActionItems } from './NotificationActions';

export type NotificationDataProps = {
  icon?: string;
  clickToClose?: boolean;
  hasCloseButton?: boolean;
  hasEllipsis?: boolean;
  themeOverride?: 'grey';
  // if left empty, the notification will have its normal colors
  labelValues?: Record<string, any>;
  hasSpinner?: boolean;
  actions?: NotificationActionItems;
};
type Props = NotificationDataProps & {
  children?: Node;
  onClose?: (...args: Array<any>) => any;
  isVisible: boolean;
  index?: number;
};
export default class Notification extends Component<Props> {
  static contextTypes = { intl: intlShape.isRequired };

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
    const Container = isClickToClose ? 'button' : 'div';
    const notificationMessageStyles = classNames([
      styles.component,
      isVisible ? styles.isVisible : null,
      isClickToClose ? styles.clickToClose : null,
      themeOverride === 'grey' ? styles.themeOverrideGrey : null,
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
      <Container
        className={notificationMessageStyles}
        type={isClickToClose ? 'button' : undefined}
        onClick={isClickToClose ? onClose : undefined}
        role={isClickToClose ? undefined : 'status'}
        aria-live="polite"
        aria-hidden={!isVisible}
        tabIndex={isClickToClose && !isVisible ? -1 : undefined}
        style={{
          zIndex: 9999999 + (index || 0),
        }}
      >
        {isVisible && (
          <Fragment>
            {icon && <SVGInline svg={icon} className={iconStyles} />}

            <span className={messageStyles}>{children}</span>

            {!!actions && <NotificationActions actions={actions} />}

            {hasCloseButton &&
              (isClickToClose ? (
                <span className={styles.closeButton} aria-hidden="true">
                  <SVGInline svg={closeCross} />
                </span>
              ) : (
                <button
                  type="button"
                  className={styles.closeButton}
                  aria-label={this.context.intl.formatMessage(
                    globalMessages.close
                  )}
                  onClick={() => onClose && onClose()}
                >
                  <SVGInline svg={closeCross} />
                </button>
              ))}
          </Fragment>
        )}
      </Container>
    );
  }
}
