// @flow
import React from 'react';
import { render } from 'react-dom';
import { Router, hashHistory } from 'react-router';
import injectTapEventPlugin from 'react-tap-event-plugin';
import { addLocaleData } from 'react-intl';
import en from 'react-intl/locale-data/en';
import de from 'react-intl/locale-data/de';
import routes from './routes';
import './styles/index.global.scss';

// Needed for onTouchTap (http://stackoverflow.com/a/34015469/988941)
injectTapEventPlugin();

// https://github.com/yahoo/react-intl/wiki#loading-locale-data
addLocaleData([en, de]);

render(<Router history={hashHistory} routes={routes} />, document.getElementById('root'));
