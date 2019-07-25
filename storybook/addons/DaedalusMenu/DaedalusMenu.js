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
    this.setState(
      currenState => ({
        ...currenState,
        ...initialState,
      }),
      () => {
        const { themeName, localeName } = this.state;
        this.props.api.setQueryParams({ themeName, localeName });
      }
    );

  updateParam = (newParamState: Object) => this.setState(newParamState);

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
        <span style={styles.separator} />
        <div style={styles.menuSlot}>
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
        <span style={styles.separator} />
        <div style={styles.menuSlot}>
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
        <span style={styles.separator} />
      </div>
    );
  }
}
export default DaedalusMenu;
