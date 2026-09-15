import React, { Component, Fragment } from 'react';
import { IntlProvider, addLocaleData } from 'react-intl';
import en from 'react-intl/locale-data/en';
import ja from 'react-intl/locale-data/ja';
import {
  themes,
  themesIds,
  locales,
  osMinWindowHeights,
  themeNames,
  localeNames,
  osNames,
} from './config';
import translations from '../../../source/renderer/app/i18n/translations';
import ThemeManager from '../../../source/renderer/app/ThemeManager';
import WindowSizeManager from '../../../source/renderer/app/WindowSizeManager';
// // https://github.com/yahoo/react-intl/wiki#loading-locale-data
addLocaleData([...en, ...ja]);

type Props = {
  children: any;
  themeName?: string;
  localeName?: string;
  osName?: string;
};

/*
 * The theme, locale and OS selections used to live in this component's own
 * state, pushed in over an addon channel by a hand-written toolbar addon. They
 * are now Storybook globals, declared in preview.tsx and chosen from the
 * toolbar Storybook renders itself, so this component reads them rather than
 * owning them. Storybook persists a global across a reload and encodes it in
 * the story URL, which the hand-written addon did with sessionStorage and a
 * location hash.
 */
export default class StoryWrapper extends Component<Props> {
  static defaultProps = {
    themeName: themeNames[0],
    localeName: localeNames[0],
    osName: osNames[0],
  };

  render() {
    const { children: Story, themeName, localeName, osName } = this.props;
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
          {/* Stories are handed the selections as props. Storybook also puts
              them on the story context, where a story can read them directly. */}
          <Story osName={osName} locale={locale} currentTheme={themeId} />
        </IntlProvider>
      </Fragment>
    );
  }
}
