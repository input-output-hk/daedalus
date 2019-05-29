// @flow
import React, { Component } from 'react';
import classnames from 'classnames';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import styles from './DisplaySettings.scss';
import themeLightBluePreview from '../../../assets/images/themes/light-blue.png';
import themeCardanoPreview from '../../../assets/images/themes/cardano.png';
import themeDarkBluePreview from '../../../assets/images/themes/dark-blue.png';
import { THEMES } from '../../../themes/index';

const messages = defineMessages({
  themeLabel: {
    id: 'settings.display.themeLabel',
    defaultMessage: '!!!Theme',
    description:
      'Label for the "Theme" selection on the display settings page.',
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
  themeOrange: {
    id: 'settings.display.themeNames.orange',
    defaultMessage: 'Orange',
    description: 'Name of the "Orange" theme on the display settings page.',
  },
  themePurple: {
    id: 'settings.display.themeNames.purple',
    defaultMessage: 'Purple',
    description: 'Name of the "Purple" theme on the display settings page.',
  },
  themeTurquoise: {
    id: 'settings.display.themeNames.turquoise',
    defaultMessage: 'Turquoise',
    description: 'Name of the "Turquoise" theme on the display settings page.',
  },
});

type Props = {
  theme: string,
  selectTheme: Function,
};

@observer
export default class DisplaySettings extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { theme, selectTheme } = this.props;
    const { intl } = this.context;

    const themeLightBlueClasses = classnames([
      theme === THEMES.LIGHT_BLUE ? styles.active : styles.inactive,
      styles.themeImageWrapper,
    ]);

    const themeCardanoClasses = classnames([
      theme === THEMES.CARDANO ? styles.active : styles.inactive,
      styles.themeImageWrapper,
    ]);

    const themeDarkBlueClasses = classnames([
      theme === THEMES.DARK_BLUE ? styles.active : styles.inactive,
      styles.themeImageWrapper,
    ]);

    return (
      <div className={styles.component}>
        <div className={styles.label}>
          {intl.formatMessage(messages.themeLabel)}
        </div>

        <div className={styles.themesWrapper}>
          <button
            className={themeLightBlueClasses}
            onClick={selectTheme.bind(this, { theme: THEMES.LIGHT_BLUE })}
          >
            <img
              src={themeLightBluePreview}
              role="presentation"
              draggable="false"
            />
            <span>{intl.formatMessage(messages.themeLightBlue)}</span>
          </button>

          <button
            className={themeCardanoClasses}
            onClick={selectTheme.bind(this, { theme: THEMES.CARDANO })}
          >
            <img
              src={themeCardanoPreview}
              role="presentation"
              draggable="false"
            />
            <span>{intl.formatMessage(messages.themeCardano)}</span>
          </button>

          <button
            className={themeDarkBlueClasses}
            onClick={selectTheme.bind(this, { theme: THEMES.DARK_BLUE })}
          >
            <img
              src={themeDarkBluePreview}
              role="presentation"
              draggable="false"
            />
            <span>{intl.formatMessage(messages.themeDarkBlue)}</span>
          </button>

          <button
            className={themeDarkBlueClasses}
            onClick={selectTheme.bind(this, { theme: THEMES.ORANGE })}
          >
            <div
              style={{
                backgroundColor: '#ff8c1a',
                width: '100px',
                height: '50px',
              }}
            />
            <span>{intl.formatMessage(messages.themeOrange)}</span>
          </button>

          <button
            className={themeDarkBlueClasses}
            onClick={selectTheme.bind(this, { theme: THEMES.PURPLE })}
          >
            <div
              style={{
                backgroundColor: '#ccccff',
                width: '100px',
                height: '50px',
              }}
            />
            <span>{intl.formatMessage(messages.themePurple)}</span>
          </button>

          <button
            className={themeDarkBlueClasses}
            onClick={selectTheme.bind(this, { theme: THEMES.TURQUOISE })}
          >
            <div
              style={{
                backgroundColor: '#006666',
                width: '100px',
                height: '50px',
              }}
            />
            <span>{intl.formatMessage(messages.themeTurquoise)}</span>
          </button>
        </div>
      </div>
    );
  }
}
