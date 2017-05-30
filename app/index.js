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
import CardanoClientApi from './api/CardanoClientApi';
import environment from './environment';
import setupStores from './stores';
import actions from './actions';
import Action from './actions/lib/Action';
import translations from './i18n/translations';
import './themes/index.global.scss';
import patchCardanoApi from './api/mocks/patchCardanoApi';

// run MobX in strict mode
useStrict(true);

// https://github.com/yahoo/react-intl/wiki#loading-locale-data
addLocaleData([en, de, hr, ja]);

const initializeDaedalus = () => {
  const api = new CardanoClientApi();
  const router = new RouterStore();
  const history = syncHistoryWithStore(hashHistory, router);
  const stores = setupStores(api, actions, router);
  window.daedalus = {
    api,
    environment,
    actions,
    stores,
    translations,
    reset: action(() => {
      Action.resetAllActions();
      api.reset();
      setupStores(api, actions, router);
    }),
    test: {
      patchCardanoApi
    }
  };
  render((
    <App stores={stores} actions={actions} history={history} />
  ), document.getElementById('root'));
};

window.addEventListener('load', initializeDaedalus);
window.addEventListener('dragover', (event) => event.preventDefault());
window.addEventListener('drop', (event) => event.preventDefault());
