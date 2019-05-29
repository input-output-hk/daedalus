// @flow
import React, { Component } from 'react';
import styles from './DaedalusMenu.scss';

type Props = {
  localeNames: Array<string>,
  themeNames: Array<string>,
  setLocaleName: Function,
  setThemeName: Function,
  currentLocale: string,
  currentTheme: string,
};

class DaedalusMenu extends Component<Props> {
  render() {
    const {
      localeNames,
      themeNames,
      setLocaleName,
      setThemeName,
      currentLocale,
      currentTheme,
    } = this.props;

    return (
      <div className={styles.component}>
        <div>
          <h2>Language:</h2>
          {localeNames.map(localeItem => (
            <button
              key={localeItem}
              onClick={() => setLocaleName(localeItem)}
              className={currentLocale === localeItem ? styles.selected : null}
            >
              {localeItem}
            </button>
          ))}
        </div>
        <div>
          <h2>Theme:</h2>
          {themeNames.map(themeItem => (
            <button
              key={themeItem}
              onClick={() => setThemeName(themeItem)}
              className={currentTheme === themeItem ? styles.selected : null}
            >
              {themeItem}
            </button>
          ))}
        </div>
      </div>
    );
  }
}

export default DaedalusMenu;
