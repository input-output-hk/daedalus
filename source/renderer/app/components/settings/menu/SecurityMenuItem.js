// @flow
import React from 'react';
import classnames from 'classnames';
import { observer } from 'mobx-react';
import { injectIntl } from 'react-intl';
import styles from './SettingsMenu.scss';
import messages from './SettingsMenu.messages';
import SettingsMenuItem from './SettingsMenuItem';
import NotificationDot from '../../widgets/notification-dot/NotificationDot';
import { ROUTES } from '../../../routes-config';
import { useTriggerOnRouteLeave } from './hooks';
import { useDiscreetModeFeature } from '../../../features/discreet-mode';
import type { Intl } from '../../../types/i18nTypes';

type Props = {
  intl: Intl,
  currentLocale: string,
  isActiveItem: Function,
  onClick: Function,
};

const SecurityMenuItem = ({
  intl,
  isActiveItem,
  currentLocale,
  onClick,
}: Props) => {
  const {
    isNotificationEnabled,
    setDiscreetModeNotification,
    setDiscreetModeSettingsTooltip,
  } = useDiscreetModeFeature();
  const disableNotification = () => {
    setDiscreetModeNotification(false);
    setDiscreetModeSettingsTooltip(false);
  };
  const isJapanese = currentLocale === 'ja-JP';
  const isActive = isActiveItem(ROUTES.SETTINGS.SECURITY);

  useTriggerOnRouteLeave({
    enabled: isNotificationEnabled,
    isOnRoute: isActive,
    onLeave: disableNotification,
  });

  return (
    <NotificationDot
      enabled={isNotificationEnabled}
      dotClassName={classnames(
        styles.dot,
        isActive && styles.active,
        isJapanese && styles.jp
      )}
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
