// @flow
import React from 'react';
import { action, useStrict } from 'mobx';
import { render } from 'react-dom';
import { addLocaleData } from 'react-intl';
import en from 'react-intl/locale-data/en';
import de from 'react-intl/locale-data/de';
import hr from 'react-intl/locale-data/hr';
import ja from 'react-intl/locale-data/ja';
import { RouterStore, syncHistoryWithStore } from 'mobx-react-router';
import { hashHistory } from 'react-router';
import App from './App';
import About from './About';
import environment from './environment';
import setupStores from './stores';
import actions from './actions';
import utils from './utils';
import Action from './actions/lib/Action';
import translations from './i18n/translations';
import './themes/index.global.scss';
import { getUrlParameterByName } from './utils/routing';
import { setupApi } from './api/index';

// run MobX in strict mode
useStrict(true);

// https://github.com/yahoo/react-intl/wiki#loading-locale-data
addLocaleData([en, de, hr, ja]);

// Use test env if the 'test' url param is set to 'true'
const isInjectedTestEnv = getUrlParameterByName('test') === 'true';
const isAboutWindow = getUrlParameterByName('window') === 'about';
if (isInjectedTestEnv) environment.current = environment.TEST;

const initializeDaedalus = () => {
  const api = setupApi();
  const router = new RouterStore();
  const history = syncHistoryWithStore(hashHistory, router);
  const stores = setupStores(api, actions, router);
  window.daedalus = {
    api,
    environment,
    actions,
    utils,
    stores,
    translations,
    reset: action(() => {
      Action.resetAllActions();
      setupStores(api, actions, router);
    }),
  };

  const rootElement = document.getElementById('root');
  if (!rootElement) throw new Error('No #root element found.');
  if (isAboutWindow) {
    render(<About stores={stores} />, rootElement);
  } else {
    render(<App stores={stores} actions={actions} history={history} />, rootElement);
  }
};

window.addEventListener('load', initializeDaedalus);
window.addEventListener('dragover', (event) => event.preventDefault());
window.addEventListener('drop', (event) => event.preventDefault());
