import React from 'react';
import classnames from 'classnames';
import { observer } from 'mobx-react';
import { injectIntl } from 'react-intl';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './SettingsMenu.scss' or its co... Remove this comment to see the full error message
import styles from './SettingsMenu.scss';
import messages from './SettingsMenu.messages';
import SettingsMenuItem from './SettingsMenuItem';
import NotificationDot from '../../widgets/notification-dot/NotificationDot';
import { ROUTES } from '../../../routes-config';
import { useTriggerOnRouteLeave } from './hooks';
import { useDiscreetModeFeature } from '../../../features/discreet-mode';
import type { Intl } from '../../../types/i18nTypes';

type Props = {
  intl: Intl;
  isActiveItem: (...args: Array<any>) => any;
  onClick: (...args: Array<any>) => any;
};

const SecurityMenuItem = ({ intl, isActiveItem, onClick }: Props) => {
  const {
    isNotificationEnabled,
    setDiscreetModeNotification,
    setDiscreetModeSettingsTooltip,
  } = useDiscreetModeFeature();

  const disableNotification = () => {
    setDiscreetModeNotification(false);
    setDiscreetModeSettingsTooltip(false);
  };

  const isActive = isActiveItem(ROUTES.SETTINGS.SECURITY);
  useTriggerOnRouteLeave({
    enabled: isNotificationEnabled,
    isOnRoute: isActive,
    onLeave: disableNotification,
  });
  return (
    <NotificationDot
      enabled={isNotificationEnabled}
      dotClassName={classnames(styles.dot, isActive && styles.active)}
    >
      <SettingsMenuItem
        active={isActive}
        label={intl.formatMessage(messages.security)}
        onClick={() => onClick(ROUTES.SETTINGS.SECURITY)}
        className="security"
      />
    </NotificationDot>
  );
};

export default injectIntl(observer(SecurityMenuItem));
