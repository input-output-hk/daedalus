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

  const theme = stores.profile.currentTheme;
  const colors = themes[theme];

  window.zE(() => window.zE.hide());
  window.zESettings = {
    webWidget: {
      color: {
        theme: colors['--theme-support-widget-header-color'],
      }
    }
  };
};
