// @flow
import React, { Component, PropTypes } from 'react';
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
import Reactions from './reactions/index';
import './styles/index.global.scss';
import setupStores from './stores';
import actions from './actions';

// https://github.com/yahoo/react-intl/wiki#loading-locale-data
addLocaleData([en, de, hr]);

@inject('stores') @observer
class Daedalus extends Component {

  static propTypes = {
    stores: PropTypes.shape({
      app: PropTypes.shape({
        currentLocale: PropTypes.string.isRequired,
      }).isRequired
    }).isRequired,
  };

  render() {
    const locale = this.props.stores.app.currentLocale;
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
  const controller = new AppController(api, stores);
  const reactions = new Reactions(controller, stores);
  window.daedalus = {
    controller,
    api,
    environment,
    ipc: ipcRenderer,
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
          <Provider controller={controller} stores={stores} actions={actions}>
            <Daedalus stores={stores} />
          </Provider>
        </Router>
      );
      render(app, document.getElementById('root'));
    }
  };
  window.daedalus.render();
};

window.addEventListener('load', initializeDaedalus);
