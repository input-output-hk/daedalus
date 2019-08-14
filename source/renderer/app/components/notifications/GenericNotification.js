// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { intlShape } from 'react-intl';
import classNames from 'classnames';
import Action from '../../actions/lib/Action';
import NotificationMessage from '../widgets/NotificationMessage';
import successIcon from '../../assets/images/success-small.inline.svg';
import spinnerIcon from '../../assets/images/spinner-dark.inline.svg';
import type { Props as NotificationMessageProps } from '../widgets/NotificationMessage';
import styles from './GenericNotification.scss';

type Props = {
  ...$Exact<NotificationMessageProps>,
  id: string,
  icon?: 'success' | 'spinner' | string,
  iconStyle?: Object,
  duration?: number,
  actionToListenAndOpen?: Action<any>,
  actionToListenAndClose?: Action<any>,
  hasEllipsis?: boolean,
  themeOverride?: 'grey', // if left empty, the noticiation will have its normal colors

  /**
   *
   * Extra props from NotificationMessageProps.js:
   *
   * {
   *   show: boolean,
   *   children?: Node,
   *   clickToClose?: boolean,
   *   hasCloseButton?: boolean,
   *   order?: 'auto' | number | 'initial' | 'inherit',
   * }
   */

  /**
   * Extra props provided by <GenericNotificationContainer />
   *
   * {
   *   openNotification: Action<any>,
   *   closeNotification: Action<any>,
   * }
   */
  openNotification?: Action<any>,
  closeNotification?: Action<any>,
};

@observer
export default class GenericNotification extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  constructor(props: Props) {
    super(props);
    const { actionToListenAndOpen, actionToListenAndClose } = this.props;
    if (actionToListenAndOpen && actionToListenAndOpen.listen) {
      actionToListenAndOpen.listen(this.openNotification);
    }
    if (actionToListenAndClose && actionToListenAndClose.listen) {
      actionToListenAndClose.listen(this.closeNotification);
    }
  }

  componentWillUnmount() {
    const { actionToListenAndOpen, actionToListenAndClose } = this.props;
    if (actionToListenAndOpen && actionToListenAndOpen.remove) {
      actionToListenAndOpen.remove(this.openNotification);
    }
    if (actionToListenAndClose && actionToListenAndClose.remove) {
      actionToListenAndClose.remove(this.closeNotification);
    }
    this.closeNotification();
  }

  openNotification = () => {
    const { openNotification, id, duration } = this.props;
    if (openNotification) openNotification.trigger({ id, duration });
  };

  closeNotification = () => {
    const { id, closeNotification } = this.props;
    if (closeNotification) closeNotification.trigger({ id });
  };

  render() {
    const {
      children,
      show,
      hasCloseButton,
      clickToClose,
      order,
      hasEllipsis,
      themeOverride,
    } = this.props;

    let { icon, iconStyle } = this.props;
    if (icon === 'success') icon = successIcon;
    if (icon === 'spinner') {
      icon = spinnerIcon;
      iconStyle = styles.spinnerIcon;
    }

    const childrenStyles = classNames([hasEllipsis ? styles.ellipsis : null]);

    return (
      <NotificationMessage
        icon={icon}
        iconStyle={iconStyle}
        show={show}
        onClose={this.closeNotification}
        hasCloseButton={hasCloseButton}
        clickToClose={clickToClose}
        order={order}
        themeOverride={themeOverride}
      >
        <div className={childrenStyles}>{children}</div>
      </NotificationMessage>
    );
  }
}
