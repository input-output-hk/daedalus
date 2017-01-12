// @flow
import React, { Component, PropTypes } from 'react';
import { action } from 'mobx';
import { render } from 'react-dom';
import { addLocaleData } from 'react-intl';
import en from 'react-intl/locale-data/en';
import de from 'react-intl/locale-data/de';
import hr from 'react-intl/locale-data/hr';
import { RouterStore, syncHistoryWithStore } from 'mobx-react-router';
import { hashHistory } from 'react-router';
import App from './App';
import StubApi from './api/StubApi';
import CardanoClientApi from './api/CardanoClientApi';
import environment from './environment';
import setupStores from './stores';
import actions from './actions';
import { resetAllActions } from './actions/lib/actions';
import translations from './i18n/translations';
import './themes/index.global.scss';

// https://github.com/yahoo/react-intl/wiki#loading-locale-data
addLocaleData([en, de, hr]);

const initializeDaedalus = () => {
  const api = environment.CARDANO_API ? new CardanoClientApi() : new StubApi();
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
      api.repository.reset();
      resetAllActions();
      setupStores(api, actions, router);
    }),
  };
  render((
    <App stores={stores} actions={actions} history={history} />
  ), document.getElementById('root'));
};

window.addEventListener('load', initializeDaedalus);
