// @flow
import React from 'react';
import { observer } from 'mobx-react';
import { injectIntl } from 'react-intl';
import classnames from 'classnames';
import SettingsMenuItem from './SettingsMenuItem';
import styles from './SettingsMenu.scss';
import { ROUTES } from '../../../routes-config';
import messages from './SettingsMenu.messages';
import { DiscreetModeFeatureInject } from '../../../features/discreet-mode';
import NotificationDot from '../../widgets/notification-dot/NotificationDot';
import type { Intl } from '../../../types/i18nTypes';
import type { Locale } from '../../../../../common/types/locales.types';

type Props = {
  intl: Intl,
  isFlight: boolean,
  isActiveItem: Function,
  currentLocale: Locale,
  onItemClick: Function,
};

const SettingsMenu = ({
  intl,
  currentLocale,
  onItemClick,
  isActiveItem,
  isFlight,
}: Props) => {
  return (
    <div>
      <div className={styles.component}>
        <SettingsMenuItem
          label={intl.formatMessage(messages.general)}
          onClick={() => onItemClick(ROUTES.SETTINGS.GENERAL)}
          active={isActiveItem(ROUTES.SETTINGS.GENERAL)}
          className="general"
        />
        <DiscreetModeFeatureInject>
          {({ isNotificationEnabled, setDiscreetModeNotification }) => (
            <NotificationDot
              enabled={isNotificationEnabled}
              dotClassName={classnames(
                styles.dot,
                currentLocale === 'ja-JP' && styles.dotJp
              )}
            >
              <SettingsMenuItem
                label={intl.formatMessage(messages.security)}
                onClick={() => {
                  if (isNotificationEnabled) {
                    setDiscreetModeNotification(false);
                  }
                  onItemClick(ROUTES.SETTINGS.SECURITY);
                }}
                active={isActiveItem(ROUTES.SETTINGS.SECURITY)}
                className="security"
              />
            </NotificationDot>
          )}
        </DiscreetModeFeatureInject>
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
