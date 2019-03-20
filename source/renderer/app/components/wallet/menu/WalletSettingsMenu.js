// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import SettingsMenuItem from './WalletSettingsMenuItem';
import styles from './WalletSettingsMenu.scss';
import { ROUTES } from '../../../routes-config';

const messages = defineMessages({
  general: {
    id: 'wallet.settings.menu.general.link.label',
    defaultMessage: '!!!General',
    description: 'Label for the "General" link in the settings menu.',
  },
  utxo: {
    id: 'wallet.settings.menu.utxo.link.label',
    defaultMessage: '!!!Wallet UTxO distribution',
    description: 'Label for the "Support" link in the settings menu.',
  },
});

type Props = {
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
    const { onItemClick, isActiveItem } = this.props;
    return (
      <div>
        <div className={styles.component}>
          <SettingsMenuItem
            label={intl.formatMessage(messages.general)}
            onClick={() => onItemClick(ROUTES.WALLETS.SETTINGS.GENERAL)}
            active={isActiveItem(ROUTES.WALLETS.SETTINGS.GENERAL)}
            className="general"
          />

          <SettingsMenuItem
            label={intl.formatMessage(messages.utxo)}
            onClick={() => onItemClick(ROUTES.WALLETS.SETTINGS.UTXO)}
            active={isActiveItem(ROUTES.WALLETS.SETTINGS.UTXO)}
            className="utxo"
          />
        </div>
      </div>
    );
  }
}
