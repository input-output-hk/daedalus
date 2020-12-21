// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import SettingsMenuItem from './SettingsMenuItem';
import styles from './SettingsMenu.scss';
import { ROUTES } from '../../../routes-config';

const messages = defineMessages({
  general: {
    id: 'settings.menu.general.link.label',
    defaultMessage: '!!!General',
    description: 'Label for the "General" link in the settings menu.',
  },
  stakePools: {
    id: 'settings.menu.stakePools.link.label',
    defaultMessage: '!!!Stake Pools',
    description: 'Label for the "Support" link in the settings menu.',
  },
  support: {
    id: 'settings.menu.support.link.label',
    defaultMessage: '!!!Support',
    description: 'Label for the "Support" link in the settings menu.',
  },
  termsOfUse: {
    id: 'settings.menu.termsOfUse.link.label',
    defaultMessage: '!!!Terms of service',
    description: 'Label for the "Terms of service" link in the settings menu.',
  },
  display: {
    id: 'settings.menu.display.link.label',
    defaultMessage: '!!!Themes',
    description: 'Label for the "Themes" link in the settings menu.',
  },
});

type Props = {
  isFlight: boolean,
  isSyncing: boolean,
  currentRoute: string,
  isActiveItem: Function,
  onItemClick: Function,
};

@observer
export default class SettingsMenu extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  componentDidUpdate() {
    const { isSyncing, currentRoute, onItemClick } = this.props;
    if (currentRoute === ROUTES.SETTINGS.STAKE_POOLS && isSyncing) {
      onItemClick(ROUTES.SETTINGS.GENERAL);
    }
  }

  render() {
    const { intl } = this.context;
    const { onItemClick, isActiveItem, isFlight, isSyncing } = this.props;

    return (
      <div>
        <div className={styles.component}>
          <SettingsMenuItem
            label={intl.formatMessage(messages.general)}
            onClick={() => onItemClick(ROUTES.SETTINGS.GENERAL)}
            active={isActiveItem(ROUTES.SETTINGS.GENERAL)}
            className="general"
          />
          {!isSyncing && (
            <SettingsMenuItem
              label={intl.formatMessage(messages.stakePools)}
              onClick={() => onItemClick(ROUTES.SETTINGS.STAKE_POOLS)}
              active={isActiveItem(ROUTES.SETTINGS.STAKE_POOLS)}
              className="stakePools"
            />
          )}
          {!isFlight && !global.isShelleyTestnet && (
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
  }
}
