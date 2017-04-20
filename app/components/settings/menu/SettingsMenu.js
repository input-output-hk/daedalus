// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import SettingsMenuItem from './SettingsMenuItem';
import styles from './SettingsMenu.scss';
import { ROUTES } from '../../../Routes';

const messages = defineMessages({
  general: {
    id: 'settings.menu.general.link.label',
    defaultMessage: '!!!General',
    description: 'Label for the "General" link in the settings menu.'
  },
});

@observer
export default class SettingsMenu extends Component {

  props: {
    isActiveItem: Function,
    onItemClick: Function,
  };

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
            onClick={() => onItemClick(ROUTES.SETTINGS.GENERAL)}
            active={isActiveItem(ROUTES.SETTINGS.GENERAL)}
            className="general"
          />
        </div>
      </div>
    );
  }

}
