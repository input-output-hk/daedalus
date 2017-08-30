// @flow
import React, { Component } from 'react';
import classnames from 'classnames';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import styles from './DisplaySettings.scss';
import themeLightBlue from '../../../assets/images/themes/light-blue.png';
import themeCardano from '../../../assets/images/themes/cardano.png';
import themeDarkBlue from '../../../assets/images/themes/dark-blue.png';

const messages = defineMessages({
  themeLabel: {
    id: 'settings.display.themeLabel',
    defaultMessage: '!!!Theme',
    description: 'Label for the "Theme" selection on the display settings page.',
  },
  themeLightBlue: {
    id: 'settings.display.themeNames.lightBlue',
    defaultMessage: '!!!Light blue',
    description: 'Name of the "Light blue" theme on the display settings page.',
  },
  themeCardano: {
    id: 'settings.display.themeNames.cardano',
    defaultMessage: '!!!Cardano',
    description: 'Name of the "Cardano" theme on the display settings page.',
  },
  themeDarkBlue: {
    id: 'settings.display.themeNames.darkBlue',
    defaultMessage: '!!!Dark blue',
    description: 'Name of the "Dark blue" theme on the display settings page.',
  },
});

@observer
export default class DisplaySettings extends Component {

  props: {
    theme: string,
    selectTheme: Function,
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { theme, selectTheme } = this.props;
    const { intl } = this.context;

    const themeLightBlueClasses = classnames([
      theme === 'light-blue' ? styles.active : styles.inactive,
      styles.themeImageWrapper,
    ]);

    const themeCardanoClasses = classnames([
      theme === 'cardano' ? styles.active : styles.inactive,
      styles.themeImageWrapper,
    ]);

    const themeDarkBlueClasses = classnames([
      theme === 'dark-blue' ? styles.active : styles.inactive,
      styles.themeImageWrapper,
    ]);

    return (
      <div className={styles.component}>

        <div className={styles.label}>
          {intl.formatMessage(messages.themeLabel)}
        </div>

        <div className={styles.themesWrapper}>

          <button className={themeLightBlueClasses} onClick={selectTheme.bind(this, { theme: 'light-blue' })}>
            <img src={themeLightBlue} role="presentation" />
            <span>{intl.formatMessage(messages.themeLightBlue)}</span>
          </button>

          <button className={themeCardanoClasses} onClick={selectTheme.bind(this, { theme: 'cardano' })}>
            <img src={themeCardano} role="presentation" />
            <span>{intl.formatMessage(messages.themeCardano)}</span>
          </button>

          <button className={themeDarkBlueClasses} onClick={selectTheme.bind(this, { theme: 'dark-blue' })}>
            <img src={themeDarkBlue} role="presentation" />
            <span>{intl.formatMessage(messages.themeDarkBlue)}</span>
          </button>

        </div>

      </div>
    );
  }

}
