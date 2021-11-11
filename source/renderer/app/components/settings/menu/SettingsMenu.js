// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { intlShape } from 'react-intl';
import SettingsMenuItem from './SettingsMenuItem';
import styles from './SettingsMenu.scss';
import { ROUTES } from '../../../routes-config';
import messages from './SettingsMenu.messages';

type Props = {
  isFlight: boolean,
  isActiveItem: Function,
  onItemClick: Function,
};

@observer
export default class SettingsMenu extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { onItemClick, isActiveItem, isFlight } = this.props;

    return (
      <div>
        <div className={styles.component}>
          <SettingsMenuItem
            label={intl.formatMessage(messages.general)}
            onClick={() => onItemClick(ROUTES.SETTINGS.GENERAL)}
            active={isActiveItem(ROUTES.SETTINGS.GENERAL)}
            className="general"
          />
          <SettingsMenuItem
            label={intl.formatMessage(messages.security)}
            onClick={() => onItemClick(ROUTES.SETTINGS.SECURITY)}
            active={isActiveItem(ROUTES.SETTINGS.SECURITY)}
            className="security"
          />
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
  }
}
