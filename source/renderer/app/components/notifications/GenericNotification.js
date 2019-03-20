// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { intlShape } from 'react-intl';
import Action from '../../actions/lib/Action';
import NotificationMessage from '../widgets/NotificationMessage';
import successIcon from '../../assets/images/success-small.inline.svg';
import type { Props as NotificationMessagePreps } from '../widgets/NotificationMessage';

type Props = {
  ...$Exact<NotificationMessagePreps>,
  id: string,
  icon?: string,
  duration?: number,
  actionToListenAndOpen?: Action<any>,
  actionToListenAndClose?: Action<any>,
  openNotification: Action<any>,
  closeNotification: Action<any>,
};

@observer
export default class GenericNotification extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static defaultProps = {
    icon: successIcon,
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
    openNotification.trigger({ id, duration });
  };

  closeNotification = () => {
    const { id, closeNotification } = this.props;
    closeNotification.trigger({ id });
  };

  render() {
    const {
      children,
      show,
      icon,
      hasCloseButton,
      clickToClose,
      order,
    } = this.props;

    return (
      <NotificationMessage
        icon={icon}
        show={show}
        onClose={this.closeNotification}
        hasCloseButton={hasCloseButton}
        clickToClose={clickToClose}
        order={order}
      >
        {children}
      </NotificationMessage>
    );
  }
}
