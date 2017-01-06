// @flow
import React, { Component } from 'react';
import { Provider, inject, observer } from 'mobx-react';
import { action } from 'mobx';
import { render } from 'react-dom';
import { addLocaleData, IntlProvider } from 'react-intl';
import { MemoryRouter as Router } from 'react-router';
import { ipcRenderer } from 'electron';
import en from 'react-intl/locale-data/en';
import de from 'react-intl/locale-data/de';
import hr from 'react-intl/locale-data/hr';
import translations from './i18n/translations';
import App from './App';
import StubApi from './api/StubApi';
import CardanoClientApi from './api/CardanoClientApi';
import environment from './environment';
import AppController from './controllers/AppController';
import appStateFactory, { appStatePropType } from './state';
import Reactions from './reactions/index';
import './styles/index.global.scss';
import setupStores from './stores';
import actions from './actions';

// https://github.com/yahoo/react-intl/wiki#loading-locale-data
addLocaleData([en, de, hr]);

@inject('state') @observer
class Daedalus extends Component {

  static propTypes = {
    state: appStatePropType.isRequired,
  };

  render() {
    const { state } = this.props;
    const locale = state.i18n.locale;
    return (
      <IntlProvider {...{ locale, key: locale, messages: translations[locale] }}>
        <App />
      </IntlProvider>
    );
  }
}

const initializeDaedalus = () => {
  const api = environment.WITH_CARDANO_API ? new CardanoClientApi() : new StubApi();
  const stores = setupStores(api, actions);
  const state = appStateFactory(stores);
  const controller = new AppController(state, api, stores);
  const reactions = new Reactions(state, controller, stores);
  window.daedalus = {
    controller,
    api,
    environment,
    ipc: ipcRenderer,
    state,
    actions,
    stores,
    reactions,
    reset: action(() => {
      api.repository.reset();
      setupStores(api, actions);
      controller.reset();
      window.daedalus.render();
    }),
    render() {
      const app = (
        <Router>
          <Provider state={state} controller={controller} stores={stores} actions={actions}>
            <Daedalus state={state} />
          </Provider>
        </Router>
      );
      render(app, document.getElementById('root'));
    }
  };
  window.daedalus.render();
};

window.addEventListener('load', initializeDaedalus);
