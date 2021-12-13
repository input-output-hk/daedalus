import React, { Component } from 'react';
import { set } from 'lodash';
import styles from './DaedalusMenuStyles';
import {
  themeNames,
  localeNames,
  osNames,
  getInitialState,
} from '../../stories/_support/config';

/* eslint-disable no-restricted-globals */
type Props = {
  api: Record<string, any>;
};
export type DaedalusMenuState = {
  themeName?: string;
  localeName?: string;
  osName?: string;
};

class DaedalusMenu extends Component<Props, DaedalusMenuState> {
  constructor(props: Props) {
    super(props);
    const { themeName, localeName, osName } = getInitialState();
    this.state = {
      themeName,
      localeName,
      osName,
    };
    this.props.api.on('daedalusMenu/paramUpdated', this.handleUpdateParam);
  }

  get params() {
    const { hash, search } = parent.window.location;
    const queries = hash || search;
    return new URLSearchParams(queries.slice(1));
  }

  componentWillUnmount() {
    this.props.api.off('daedalusMenu/paramUpdated', this.handleUpdateParam);
  }

  sendUpdateParam = (param: string, value: string) => {
    this.props.api.emit('daedalusMenu/updateParam', {
      param,
      value,
    });
  };
  handleUpdateParam = ({ param, value }: Record<string, any>) => {
    const query = set({}, param, value);
    this.setState(query);
    this.setHashParam(param, value);
    sessionStorage.setItem(param, value);
    this.props.api.setQueryParams(query);
  };
  setHashParam = (param: string, value: string) => {
    const hash = this.params;
    hash.delete('path');
    hash.set(param, value);
    // @ts-ignore ts-migrate(2322) FIXME: Type 'URLSearchParams' is not assignable to type '... Remove this comment to see the full error message
    parent.window.location.hash = hash;
  };
  handleSetParam = (param: string, value: string) => {
    const query = set({}, param, value);
    this.setState(query);
    this.setHashParam(param, value);
    sessionStorage.setItem(param, value); // updateParam(query);
  };

  render() {
    const { themeName, localeName, osName } = this.state;
    return (
      <div style={styles.component}>
        <span style={styles.separator} />
        <div style={styles.menuSlot}>
          {localeNames.map((localeItem) => (
            <button
              key={localeItem}
              onClick={() => this.sendUpdateParam('localeName', localeItem)}
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
          {themeNames.map((themeItem) => (
            <button
              key={themeItem}
              onClick={() => this.sendUpdateParam('themeName', themeItem)}
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
        <div style={styles.menuSlot}>
          {osNames.map((osItem) => (
            <button
              key={osItem}
              onClick={() => this.sendUpdateParam('osName', osItem)}
              style={{
                ...styles.button,
                ...(osName === osItem ? styles.selected : {}),
              }}
            >
              {osItem}
            </button>
          ))}
        </div>
        <span style={styles.separator} />
      </div>
    );
  }
}

export default DaedalusMenu;
