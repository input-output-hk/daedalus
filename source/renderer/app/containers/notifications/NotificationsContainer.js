// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { intlShape } from 'react-intl';
import type { InjectedProps } from '../../types/injectedPropsType';
import Notification from '../../components/notifications/Notification';
import type { NotificationMessageProps } from '../../components/notifications/Notification';
import messages from '../../i18n/notification-messages';

import successIcon from '../../assets/images/success-small.inline.svg';
import spinnerIcon from '../../assets/images/spinner-dark.inline.svg';

const ICONS = {
  successIcon,
  spinnerIcon,
};

@inject('stores', 'actions')
@observer
export default class NotificationsContainer extends Component<InjectedProps> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static defaultProps = { actions: null, stores: null };

  getIcon = (icon?: string) => (icon ? ICONS[icon] : icon);

  render() {
    const { stores } = this.props;
    const { activeNotifications } = stores.uiNotifications;
    const { intl } = this.context;
    return (
      <div>
        {Object.entries(activeNotifications).map(
          ([id: string, notification: NotificationMessageProps]) => (
            <Notification
              key={id}
              {...notification}
              label={intl.formatMessage(messages[id])}
              icon={this.getIcon}
            />
          )
        )}
      </div>
    );
  }
}
