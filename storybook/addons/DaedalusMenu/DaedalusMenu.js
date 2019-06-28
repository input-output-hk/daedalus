// @flow
import React, { Component } from 'react';
import { set } from 'lodash';
import styles from './DaedalusMenuStyles';

/* eslint-disable no-restricted-globals */

type Props = {
  api: Object,
};

type State = {
  localeNames: Array<string>,
  themeNames: Array<string>,
  themeName?: string,
  localeName?: string,
};

class DaedalusMenu extends Component<Props, State> {
  state = {
    localeNames: [],
    themeNames: [],
    themeName: '',
    localeName: '',
  };

  componentDidMount() {
    const { api } = this.props;
    api.on('daedalusMenu/init', this.init);
    api.on('daedalusMenu/updateParam', this.updateParam);
  }

  componentWillUnmount() {
    const { api } = this.props;
    api.on('daedalusMenu/init', this.init);
    api.on('daedalusMenu/updateParam', this.updateParam);
  }

  init = (initialState: State) =>
    this.setState(currenState => ({
      ...currenState,
      ...initialState,
    }));

  updateParam = (newParamState: Object) => this.setState(newParamState);

  get params() {
    return new URLSearchParams(parent.window.location.search);
  }

  handleSetParam = (param: string, value: string) => {
    const { api } = this.props;
    const query = set({}, param, value);
    api.setQueryParams(query);
    api.emit('daedalusMenu/receiveParam', { param, value });
  };

  render() {
    const { localeNames, themeNames, themeName, localeName } = this.state;

    return (
      <div style={styles.component}>
        <div style={styles.content}>
          <div style={styles.menuSlot}>
            <h2 style={styles.title}>Language:</h2>
            {localeNames.map(localeItem => (
              <button
                key={localeItem}
                onClick={() => this.handleSetParam('localeName', localeItem)}
                style={{
                  ...styles.button,
                  ...(localeName === localeItem ? styles.selected : {}),
                }}
              >
                {localeItem}
              </button>
            ))}
          </div>
          <div style={styles.menuSlot}>
            <h2 style={styles.title}>Theme:</h2>
            {themeNames.map(themeItem => (
              <button
                key={themeItem}
                onClick={() => this.handleSetParam('themeName', themeItem)}
                style={{
                  ...styles.button,
                  ...(themeName === themeItem ? styles.selected : {}),
                }}
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
