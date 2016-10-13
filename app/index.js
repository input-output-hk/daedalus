// @flow
import React from 'react';
import { render } from 'react-dom';
import { Router, hashHistory } from 'react-router';
import injectTapEventPlugin from 'react-tap-event-plugin';
import routes from './routes';
import './styles/index.global.scss';

// Needed for onTouchTap (http://stackoverflow.com/a/34015469/988941)
injectTapEventPlugin();

render(<Router history={hashHistory} routes={routes} />, document.getElementById('root'));
