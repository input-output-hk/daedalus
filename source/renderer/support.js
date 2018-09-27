// @flow
import { ipcRenderer } from 'electron';
import { SUPPORT_WINDOW } from '../common/ipc-api';

type Info = {
  locale: string,
  themeVars: {
    '--theme-support-widget-header-color': string
  }
};

const locales = {
  'en-US': 'en-US',
  'ja-JP': 'ja',
};

ipcRenderer.on(SUPPORT_WINDOW.INFO, (event, { locale, themeVars }: Info) => {

  window.zE(() => {
    // window.zE.hide();
    if (locale !== 'en-US') window.zE.setLocale(locales[locale]);
    window.zE.activate();
  });

  window.zESettings = {
    webWidget: {
      color: {
        theme: themeVars['--theme-support-widget-header-color'],
      }
    }
  };

});

ipcRenderer.on(SUPPORT_WINDOW.CLOSE, () => window.close());
