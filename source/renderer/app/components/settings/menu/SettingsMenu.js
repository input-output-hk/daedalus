// @flow
import React from 'react';
import { observer } from 'mobx-react';
import { injectIntl } from 'react-intl';
import SettingsMenuItem from './SettingsMenuItem';
import styles from './SettingsMenu.scss';
import { ROUTES } from '../../../routes-config';
import messages from './SettingsMenu.messages';
import { useDiscreetModeFeature } from '../../../features/discreet-mode';
import NotificationDot from '../../widgets/notification-dot/NotificationDot';
import type { Intl } from '../../../types/i18nTypes';

type Props = {
  intl: Intl,
  isFlight: boolean,
  isActiveItem: Function,
  onItemClick: Function,
};

const SettingsMenu = ({ intl, onItemClick, isActiveItem, isFlight }: Props) => {
  const {
    isNotificationEnabled,
    setDiscreetModeNotification,
  } = useDiscreetModeFeature();
  return (
    <div>
      <div className={styles.component}>
        <SettingsMenuItem
          label={intl.formatMessage(messages.general)}
          onClick={() => onItemClick(ROUTES.SETTINGS.GENERAL)}
          active={isActiveItem(ROUTES.SETTINGS.GENERAL)}
          className="general"
        />
        <NotificationDot
          enabled={isNotificationEnabled}
          dotClassName={styles.dot}
        >
          <SettingsMenuItem
            label={intl.formatMessage(messages.security)}
            onClick={() => {
              setDiscreetModeNotification(false);
              onItemClick(ROUTES.SETTINGS.SECURITY);
            }}
            active={isActiveItem(ROUTES.SETTINGS.SECURITY)}
            className="security"
          />
        </NotificationDot>
        <SettingsMenuItem
          label={intl.formatMessage(messages.wallets)}
          onClick={() => onItemClick(ROUTES.SETTINGS.WALLETS)}
          active={isActiveItem(ROUTES.SETTINGS.WALLETS)}
          className="wallets"
        />
        <SettingsMenuItem
          label={intl.formatMessage(messages.stakePools)}
          onClick={() => onItemClick(ROUTES.SETTINGS.STAKE_POOLS)}
          active={isActiveItem(ROUTES.SETTINGS.STAKE_POOLS)}
          className="stakePools"
        />
        {!isFlight && (
          <SettingsMenuItem
            label={intl.formatMessage(messages.display)}
            onClick={() => onItemClick(ROUTES.SETTINGS.DISPLAY)}
            active={isActiveItem(ROUTES.SETTINGS.DISPLAY)}
            className="display"
          />
        )}
        <SettingsMenuItem
          label={intl.formatMessage(messages.termsOfUse)}
          onClick={() => onItemClick(ROUTES.SETTINGS.TERMS_OF_USE)}
          active={isActiveItem(ROUTES.SETTINGS.TERMS_OF_USE)}
          className="termsOfService"
        />
        <SettingsMenuItem
          label={intl.formatMessage(messages.support)}
          onClick={() => onItemClick(ROUTES.SETTINGS.SUPPORT)}
          active={isActiveItem(ROUTES.SETTINGS.SUPPORT)}
          className="support"
        />
      </div>
    </div>
  );
};

export default injectIntl(observer(SettingsMenu));
