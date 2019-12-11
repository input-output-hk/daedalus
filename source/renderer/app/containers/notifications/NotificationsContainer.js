// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { intlShape, FormattedHTMLMessage } from 'react-intl';
import type { InjectedProps } from '../../types/injectedPropsType';
import Notification from '../../components/notifications/Notification';
import type { StoredNotification } from '../../types/notificationType';
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

  getLabel = (id: string, labelValues?: ?Object) => {
    return labelValues ? (
      <FormattedHTMLMessage {...messages[id]} values={labelValues} />
    ) : (
      this.context.formatMessage(messages[id])
    );
  };
  render() {
    const { stores, actions } = this.props;
    const { closeNotification } = actions.notifications;
    const { activeNotifications } = stores.uiNotifications;
    return (
      <div>
        {Object.keys(activeNotifications).map(key => {
          const {
            notificationConfig: { id },
            notificationMessage,
            labelValues,
          } = activeNotifications[key];
          return (
            <Notification
              key={id}
              {...notificationMessage}
              label={this.getLabel(id, labelValues)}
              onClose={() => closeNotification.trigger({ id })}
              icon={successIcon}
            />
          );
        })}
      </div>
    );
  }
}
