// @flow
import React from 'react';
import { render } from 'react-dom';
import { addLocaleData } from 'react-intl';
import { MemoryRouter as Router } from 'react-router';
import en from 'react-intl/locale-data/en';
import de from 'react-intl/locale-data/de';
import hr from 'react-intl/locale-data/hr';
import App from './App';
import api from './api';
import environment from './environment';
import state from './state';
import AppController from './controllers/AppController';
import './styles/index.global.scss';

// https://github.com/yahoo/react-intl/wiki#loading-locale-data
addLocaleData([en, de, hr]);

const appState = state();
const controller = new AppController(appState);
const routedApp = (
  <Router><App state={appState} controller={controller} /></Router>
);

window.daedalus = {
  controller,
  api,
  environment,
  state: appState,
  render() {
    render(routedApp, document.getElementById('root'));
  }
};

window.addEventListener('load', window.daedalus.render);
