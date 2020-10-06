// @flow
import React, { Component } from 'react';
import { EXISTING_THEME_OUTPUTS_OBJ } from '../../../source/renderer/app/themes/daedalus/index';
import styles from './ThemesManagerStyles';
import {
  /* themeNames, */ getInitialState,
} from '../../stories/_support/config';

console.log('styles', styles);
const themesMap = {
  Cardano: 'cardano.js',
  DarkBlue: 'dark-blue.js',
  DarkCardano: 'dark-cardano.js',
  FlightCandidate: 'flight-candidate.js',
  IncentivizedTestnet: 'incentivized-testnet.js',
  LightBlue: 'light-blue.js',
  ShelleyTestnet: 'shelley-testnet.js',
  White: 'white.js',
  Yellow: 'yellow.js',
};

type Props = {
  api: Object,
};

export type ThemesManagerState = {
  themeName: string,
  filter: string,
};

export default class ThemesManager extends Component<
  Props,
  ThemesManagerState
> {
  constructor(props: Props) {
    super(props);
    const { themeName } = getInitialState();
    this.state = {
      themeName,
      filter: '',
    };
    this.props.api.on('daedalusMenu/paramUpdated', this.handleUpdateParam);
  }

  handleUpdateParam = ({ param, value }: Object) => {
    if (param === 'themeName') {
      this.setState({ themeName: value });
    }
  };

  getThemeObj = () => {
    const { themeName } = this.state;
    const themeFileName = themesMap[themeName];
    return EXISTING_THEME_OUTPUTS_OBJ[themeFileName];
  };

  getThemeContent = () => {
    const theme = this.getThemeObj();
    const { filter } = this.state;
    const content = Object.values(theme).reduce((obj, item) => {
      const filteredItem = Object.entries(item).reduce(
        (filteredObj, [itemKey, itemValue]) => {
          if (itemKey.includes(filter)) filteredObj[itemKey] = itemValue;
          return filteredObj;
        },
        {}
      );
      return {
        ...obj,
        ...filteredItem,
      };
    }, {});
    return content;
  };

  render() {
    const { themeName, filter } = this.state;
    const themeContent = this.getThemeContent();
    return (
      <div style={styles.component}>
        <h2>{themeName}</h2>
        <input
          value={filter}
          onChange={event => {
            const { value } = event.target;
            this.setState({
              filter: value,
            });
          }}
          style={styles.input}
        />
        <ul style={styles.list}>
          {Object.entries(themeContent).map(([paramName, paramColor]) => (
            <li key={paramName} style={styles.listItem}>
              <em style={{ ...styles.itemColor, background: paramColor }} />
              {paramName}
            </li>
          ))}
        </ul>
      </div>
    );
  }
}
