import React, { Component, Fragment } from 'react';
import { set } from 'lodash';
import { IntlProvider, addLocaleData } from 'react-intl';
import en from 'react-intl/locale-data/en';
import ja from 'react-intl/locale-data/ja';
import { onReceiveParam, setInitialState } from '../../addons/DaedalusMenu';
import {
  getInitialState,
  themes,
  themesIds,
  locales,
  osMinWindowHeights,
} from './config';
import translations from '../../../source/renderer/app/i18n/translations';
import ThemeManager from '../../../source/renderer/app/ThemeManager';
import WindowSizeManager from '../../../source/renderer/app/WindowSizeManager';
// // https://github.com/yahoo/react-intl/wiki#loading-locale-data
addLocaleData([...en, ...ja]);
type Props = {
  children: any;
};
type State = {
  themeName: string;
  localeName: string;
  osName: string;
};
export default class StoryWrapper extends Component<Props, State> {
  constructor(props: Props) {
    super(props);
    onReceiveParam(this.handleReceiveParam);
    const { themeName, localeName, osName } = getInitialState();
    this.state = {
      themeName,
      localeName,
      osName,
    };
    setInitialState(this.state);
  }

  handleReceiveParam = ({
    param,
    value,
  }: {
    param: Array<any> | string;
    value: any;
  }) => this.setState(set({}, param, value));

  render() {
    const { children: Story } = this.props;
    const { themeName, localeName, osName } = this.state;
    if (!themeName || !localeName || !osName) return <div>LOADING</div>;
    const theme = themes[themeName];
    const themeId = themesIds[themeName];
    const locale = locales[localeName];
    const minScreenHeight = osMinWindowHeights[osName];
    return (
      <Fragment>
        {/* @ts-ignore ts-migrate(2769) FIXME: No overload matches this call. */}
        <ThemeManager variables={theme} />
        {/* @ts-ignore ts-migrate(2769) FIXME: No overload matches this call. */}
        <WindowSizeManager minScreenHeight={minScreenHeight} />
        <IntlProvider
          {...{
            locale,
            key: locale,
            messages: translations[locale],
          }}
        >
          <Story
            osName={this.state.osName}
            locale={locale}
            currentTheme={themeId}
          />
        </IntlProvider>
      </Fragment>
    );
  }
}
