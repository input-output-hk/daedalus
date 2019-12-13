// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { FormattedHTMLMessage } from 'react-intl';
import type { InjectedProps } from '../../types/injectedPropsType';
import Notification from '../../components/notifications/Notification';
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
  static defaultProps = { actions: null, stores: null };

  getIcon = (icon?: string) => (icon ? ICONS[icon] : icon);

  getLabel = (id: string, labelValues?: ?Object = {}) => (
    <FormattedHTMLMessage {...messages[id]} values={labelValues} />
  );

  render() {
    const { stores, actions } = this.props;
    const { closeNotification } = actions.notifications;
    const { notifications, activeNotifications } = stores.uiNotifications;
    return (
      <div>
        {Object.keys(notifications).map(key => {
          const {
            config: { id },
            message,
          } = notifications[key];
          const isVisible = !!activeNotifications[id];
          const { labelValues } = isVisible ? activeNotifications[key] : {};
          return (
            <Notification
              key={id}
              {...message}
              onClose={() => closeNotification.trigger({ id })}
              icon={successIcon}
              isVisible={isVisible}
            >
              {isVisible ? this.getLabel(id, labelValues) : null}
            </Notification>
          );
        })}
      </div>
    );
  }
}
