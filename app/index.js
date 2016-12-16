// @flow
import React, { Component } from 'react';
import { Provider, observer } from 'mobx-react';
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
import environment from './environment';
import AppController from './controllers/AppController';
import appStateFactory, { appStatePropType } from './state';
import './styles/index.global.scss';

// https://github.com/yahoo/react-intl/wiki#loading-locale-data
addLocaleData([en, de, hr]);

@observer(['state'])
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
  const state = appStateFactory();
  const api = new StubApi();
  const controller = new AppController(state, api);
  window.daedalus = {
    controller,
    api,
    environment,
    ipc: ipcRenderer,
    state,
    reset: action(() => {
      api.repository.reset();
      controller.reset();
      window.daedalus.render();
    }),
    render() {
      const app = (
        <Router>
          <Provider state={state} controller={controller}>
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
