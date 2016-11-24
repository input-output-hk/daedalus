// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import SettingsMenuItem from './SettingsMenuItem';
import styles from './SettingsMenu.scss';

const messages = defineMessages({
  profile: {
    id: 'settings.menu.profile.link.label',
    defaultMessage: '!!!Profile',
    description: 'Label for the "Profile" link in the settings menu.'
  },
  security: {
    id: 'settings.menu.security.link.label',
    defaultMessage: '!!!Security',
    description: 'Label for the "Security" link in the settings menu.'
  },
  identityAndVerification: {
    id: 'settings.menu.identity.and.verification.link.label',
    defaultMessage: '!!!Identity and verification',
    description: 'Label for the "Identity and verification" link in the settings menu.'
  },
  display: {
    id: 'settings.menu.display.link.label',
    defaultMessage: '!!!Display',
    description: 'Label for the "Display" link in the settings menu.'
  },
  privacy: {
    id: 'settings.menu.privacy.link.label',
    defaultMessage: '!!!Privacy',
    description: 'Label for the "Privacy" link in the settings menu.'
  },
  termsOfUse: {
    id: 'settings.menu.terms.of.use.link.label',
    defaultMessage: '!!!Terms of use',
    description: 'Label for the "Terms of use" link in the settings menu.'
  },
  support: {
    id: 'settings.menu.support.link.label',
    defaultMessage: '!!!Support',
    description: 'Label for the "Support" link in the settings menu.'
  },
});

@observer
export default class SettingsMenu extends Component {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    return (
      <div>
        <div className={styles.component}>
          <SettingsMenuItem
            label={intl.formatMessage(messages.profile)}
            active
          />
          <SettingsMenuItem
            label={intl.formatMessage(messages.security)}
            active={false}
          />
          <SettingsMenuItem
            label={intl.formatMessage(messages.identityAndVerification)}
            active={false}
          />
          <SettingsMenuItem
            label={intl.formatMessage(messages.display)}
            active={false}
          />
          <SettingsMenuItem
            label={intl.formatMessage(messages.privacy)}
            active={false}
          />
          <SettingsMenuItem
            label={intl.formatMessage(messages.termsOfUse)}
            active={false}
          />
          <SettingsMenuItem
            label={intl.formatMessage(messages.support)}
            active={false}
          />
        </div>
      </div>
    );
  }

}
