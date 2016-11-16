// @flow
import React from 'react';
import { render } from 'react-dom';
import { addLocaleData } from 'react-intl';
import { MemoryRouter as Router } from 'react-router';
import en from 'react-intl/locale-data/en';
import de from 'react-intl/locale-data/de';
import App from './App';
import state from './state/index';
import AppController from './controllers/AppController';
import './styles/index.global.scss';

// https://github.com/yahoo/react-intl/wiki#loading-locale-data
addLocaleData([en, de]);

const appState = state();
const controller = new AppController(appState);
controller.wallets.loadWallets();

const routedApp = (
  <Router><App state={appState} controller={controller} /></Router>
);

render(routedApp, document.getElementById('root'));
