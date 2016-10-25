// @flow
import React from 'react';
import { render } from 'react-dom';
import injectTapEventPlugin from 'react-tap-event-plugin';
import { addLocaleData } from 'react-intl';
import en from 'react-intl/locale-data/en';
import de from 'react-intl/locale-data/de';
import App from './App';
import './styles/index.global.scss';

// Needed for onTouchTap (http://stackoverflow.com/a/34015469/988941)
injectTapEventPlugin();

// https://github.com/yahoo/react-intl/wiki#loading-locale-data
addLocaleData([en, de]);

render(<App />, document.getElementById('root'));
