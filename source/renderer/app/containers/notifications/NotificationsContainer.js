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

  getIcon = (icon?: string = 'success') => (icon ? ICONS[`${icon}Icon`] : icon);

  getLabel = (id: string, labelValues?: ?Object) => {
    const values = typeof labelValues === 'object' ? labelValues : {};
    return <FormattedHTMLMessage {...messages[id]} values={values} />;
  };

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
          const { icon } = message || {};
          const hasSpinner = icon === 'spinner';
          return (
            <Notification
              key={id}
              {...message}
              onClose={() => closeNotification.trigger({ id })}
              icon={this.getIcon(icon)}
              isVisible={isVisible}
              hasSpinner={hasSpinner}
            >
              {isVisible ? this.getLabel(id, labelValues) : null}
            </Notification>
          );
        })}
      </div>
    );
  }
}
