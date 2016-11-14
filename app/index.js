// @flow
import React from 'react';
import { render } from 'react-dom';
import { addLocaleData } from 'react-intl';
import { MemoryRouter as Router } from 'react-router';
import en from 'react-intl/locale-data/en';
import de from 'react-intl/locale-data/de';
import App from './App';
import AppStore from './stores/AppStore';
import AppController from './controllers/AppController';
import './styles/index.global.scss';

// https://github.com/yahoo/react-intl/wiki#loading-locale-data
addLocaleData([en, de]);

const store = new AppStore();
const controller = new AppController(store);
controller.wallets.loadWallets();

render(<Router><App store={store} controller={controller} /></Router>, document.getElementById('root'));
