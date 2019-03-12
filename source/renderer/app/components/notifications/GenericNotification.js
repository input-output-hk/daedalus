// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { intlShape } from 'react-intl';
import Action from '../../actions/lib/Action';
import NotificationMessage from '../widgets/NotificationMessage';
import successIcon from '../../assets/images/success-small.inline.svg';

type Props = {
  id: string,
  message: string,
  duration: number,
  show: boolean,
  actionToListen?: Action<any>,
  openNotification: Action<any>,
  closeNotification: Action<any>,
  icon?: string,
  hasCloseButton?: boolean
};

@observer
export default class GenericNotification extends Component<Props> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static defaultProps = {
    hasCloseButton: true,
  };

  constructor(props: Props) {
    super(props);
    const { actionToListen } = this.props;
    if (actionToListen) this.registerNotificationListener(actionToListen);
  }

  componentWillUnmount() {
    const { actionToListen } = this.props;
    if (actionToListen) actionToListen.remove(this.openNotification);
    this.closeNotification();
  }

  registerNotificationListener = (actionToListen: Action<any>) => {
    actionToListen.listen(this.openNotification);
  };

  openNotification = () => {
    const { openNotification, id, duration } = this.props;
    openNotification.trigger({ id, duration });
  };

  closeNotification = () => {
    const { id, closeNotification } = this.props;
    closeNotification.trigger({ id });
  }

  render() {
    const { message, show, icon, hasCloseButton } = this.props;

    return (
      <NotificationMessage
        icon={icon || successIcon}
        show={show}
        onClose={this.closeNotification}
        hasCloseButton={hasCloseButton}
        clickToClose
      >
        {message}
      </NotificationMessage>
    );
  }

}
