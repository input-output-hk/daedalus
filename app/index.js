// @flow
import React, { Component, PropTypes } from 'react';
import { Provider, observer } from 'mobx-react';
import { render } from 'react-dom';
import { addLocaleData, IntlProvider } from 'react-intl';
import { MemoryRouter as Router } from 'react-router';
import en from 'react-intl/locale-data/en';
import de from 'react-intl/locale-data/de';
import hr from 'react-intl/locale-data/hr';
import translations from './i18n/translations';
import App from './App';
import api from './api';
import environment from './environment';
import AppController from './controllers/AppController';
import appStateFactory, { appStatePropType } from './state';
import './styles/index.global.scss';

// https://github.com/yahoo/react-intl/wiki#loading-locale-data
addLocaleData([en, de, hr]);

@observer
class Daedalus extends Component {

  static propTypes = {
    state: appStatePropType,
    controller: PropTypes.instanceOf(AppController),
  };

  render() {
    const { state, controller } = this.props;
    const locale = state.i18n.locale;
    return (
      <IntlProvider {...{ locale, key: locale, messages: translations[locale] }}>
        <Provider state={state} controller={controller}>
          <App />
        </Provider>
      </IntlProvider>
    );
  }
}

const initializeDaedalus = () => {
  const appState = appStateFactory();
  const controller = new AppController(appState);
  window.daedalus = {
    controller,
    api,
    environment,
    state: appState,
    reset() {
      api.data.reset();
      initializeDaedalus();
    }
  };
  render(<Router><Daedalus state={appState} controller={controller} /></Router>, document.getElementById('root'));
};

window.addEventListener('load', initializeDaedalus);
