// @flow

type ThemeVars = {
  '--theme-support-widget-header-color': string
};

export default (locale: string, themeVars: ThemeVars) => {

  const locales = {
    'en-US': 'en-US',
    'ja-JP': 'ja',
  };

  window.zE(() => {
    window.zE.hide();
    if (locale !== 'en-US') window.zE.setLocale(locales[locale]);
  });

  window.zESettings = {
    webWidget: {
      color: {
        theme: themeVars['--theme-support-widget-header-color'],
      }
    }
  };
};
