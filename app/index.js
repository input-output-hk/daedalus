// @flow
import React from 'react';
import { render } from 'react-dom';
import { addLocaleData } from 'react-intl';
import { MemoryRouter as Router } from 'react-router';
import en from 'react-intl/locale-data/en';
import de from 'react-intl/locale-data/de';
import App from './App';
import './styles/index.global.scss';
import { loadWallets } from './actions/wallet-actions';

loadWallets();

// https://github.com/yahoo/react-intl/wiki#loading-locale-data
addLocaleData([en, de]);

render(<Router><App /></Router>, document.getElementById('root'));
