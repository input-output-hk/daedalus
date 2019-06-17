// @flow
import React, { Component } from 'react';
import classnames from 'classnames';
import styles from './DaedalusMenu.scss';

type Props = {
  localeNames: Array<string>,
  themeNames: Array<string>,
  setParam: Function,
  onToggleVisibility: Function,
  currentLocale: string,
  currentTheme: string,
  isVisible: boolean,
};

class DaedalusMenu extends Component<Props> {
  render() {
    const {
      localeNames,
      themeNames,
      setParam,
      currentLocale,
      currentTheme,
      isVisible,
      onToggleVisibility,
    } = this.props;

    const componentStyles = classnames([
      styles.component,
      isVisible ? styles.visible : styles.minized,
    ]);

    return (
      <div className={componentStyles}>
        <button
          className={styles.minimizedButton}
          onClick={onToggleVisibility}
        />
        <div className={styles.content}>
          <div className={styles.menuSlot}>
            <h2>Language:</h2>
            {localeNames.map(localeItem => (
              <button
                key={localeItem}
                onClick={() => setParam('localeName', localeItem)}
                className={
                  currentLocale === localeItem ? styles.selected : null
                }
              >
                {localeItem}
              </button>
            ))}
          </div>
          <div className={styles.menuSlot}>
            <h2>Theme:</h2>
            {themeNames.map(themeItem => (
              <button
                key={themeItem}
                onClick={() => setParam('themeName', themeItem)}
                className={currentTheme === themeItem ? styles.selected : null}
              >
                {themeItem}
              </button>
            ))}
          </div>
        </div>
      </div>
    );
  }
}

export default DaedalusMenu;
