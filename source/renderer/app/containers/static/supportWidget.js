// @flow
import cardano from '../../themes/daedalus/cardano';
import darkBlue from '../../themes/daedalus/dark-blue';
import lightBlue from '../../themes/daedalus/light-blue';
import type { StoresMap } from '../../stores/index';

export default (stores: StoresMap) => {

  const themes = {
    cardano,
    'dark-blue': darkBlue,
    'light-blue': lightBlue,
  };

  const locales = {
    'en-US': 'en-US',
    'ja-JP': 'ja',
  };

  const { currentTheme, currentLocale } = stores.profile;
  const colors = themes[currentTheme];
  const locale = locales[currentLocale];

  window.zE(() => {
    window.zE.hide();
    if (locale !== 'en-US') window.zE.setLocale(locale);
  });

  window.zESettings = {
    webWidget: {
      color: {
        theme: colors['--theme-support-widget-header-color'],
      }
    }
  };
};
