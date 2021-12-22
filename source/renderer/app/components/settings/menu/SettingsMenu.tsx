import React from 'react';
import { observer } from 'mobx-react';
import { injectIntl } from 'react-intl';
import SettingsMenuItem from './SettingsMenuItem';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './SettingsMenu.scss' or its co... Remove this comment to see the full error message
import styles from './SettingsMenu.scss';
import { ROUTES } from '../../../routes-config';
import messages from './SettingsMenu.messages';
import type { Intl } from '../../../types/i18nTypes';
import SecurityMenuItem from './SecurityMenuItem';

type Props = {
  intl: Intl;
  isFlight: boolean;
  isActiveItem: (...args: Array<any>) => any;
  onItemClick: (...args: Array<any>) => any;
};

const SettingsMenu = ({ intl, onItemClick, isActiveItem, isFlight }: Props) => {
  return (
    <div>
      <div className={styles.component}>
        <SettingsMenuItem
          label={intl.formatMessage(messages.general)}
          onClick={() => onItemClick(ROUTES.SETTINGS.GENERAL)}
          active={isActiveItem(ROUTES.SETTINGS.GENERAL)}
          className="general"
        />
        <SecurityMenuItem isActiveItem={isActiveItem} onClick={onItemClick} />
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
