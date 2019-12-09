// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { intlShape } from 'react-intl';
import classNames from 'classnames';
import Action from '../../actions/lib/Action';
import NotificationMessage from '../widgets/NotificationMessage';
import successIcon from '../../assets/images/success-small.inline.svg';
import spinnerIcon from '../../assets/images/spinner-dark.inline.svg';
import styles from './Notification.scss';

type Props = {
  actionToListenAndClose?: Action<any>,
  actionToListenAndOpen?: Action<any>,
  children?: Node,
  clickToClose?: boolean,
  closeNotification?: Action<any>,
  duration?: number,
  hasCloseButton?: boolean,
  hasEllipsis?: boolean,
  icon?: 'success' | 'spinner' | string,
  icon?: string,
  iconStyle?: Object,
  iconStyle?: Object,
  id: string,
  // onClose?: Function,
  openNotification?: Action<any>,
  order?: 'auto' | number | 'initial' | 'inherit',
  show: boolean,
  themeOverride?: 'grey', // if left empty, the noticiation will have its normal colors
  themeOverride?: 'grey', // if left empty, the noticiation will have its normal colors,
};

@observer
export default class Notification extends Component<Props> {
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
