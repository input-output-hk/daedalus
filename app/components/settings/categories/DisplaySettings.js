// @flow
import React, { Component } from 'react';
import classnames from 'classnames';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import styles from './DisplaySettings.scss';
import themeDefault from '../../../assets/images/themes/theme-default.png';
import theme1 from '../../../assets/images/themes/theme-1.png';
import theme2 from '../../../assets/images/themes/theme-2.png';

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

    const themeDefaultClasses = classnames([
      theme === 'themeDefault' ? styles.active : styles.inactive,
      styles.themeImageWrapper,
    ]);

    const themeOneClasses = classnames([
      theme === 'theme1' ? styles.active : styles.inactive,
      styles.themeImageWrapper,
    ]);

    const themeTwoClasses = classnames([
      theme === 'theme2' ? styles.active : styles.inactive,
      styles.themeImageWrapper,
    ]);

    return (
      <div className={styles.component}>

        <div className={styles.label}>
          {intl.formatMessage(messages.themeLabel)}
        </div>

        <div className={styles.themesWrapper}>

          <button className={themeDefaultClasses} onClick={selectTheme.bind(this, { theme: 'themeDefault' })}>
            <img src={themeDefault} role="presentation" />
            <span>{intl.formatMessage(messages.themeLightBlue)}</span>
          </button>

          <button className={themeOneClasses} onClick={selectTheme.bind(this, { theme: 'theme1' })}>
            <img src={theme1} role="presentation" />
            <span>{intl.formatMessage(messages.themeCardano)}</span>
          </button>

          <button className={themeTwoClasses} onClick={selectTheme.bind(this, { theme: 'theme2' })}>
            <img src={theme2} role="presentation" />
            <span>{intl.formatMessage(messages.themeDarkBlue)}</span>
          </button>

        </div>

      </div>
    );
  }

}
