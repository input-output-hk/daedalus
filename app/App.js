// @flow
import React, { Component, PropTypes } from 'react';
import { Provider, observer } from 'mobx-react';
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';
import getMuiTheme from 'material-ui/styles/getMuiTheme';
import { IntlProvider } from 'react-intl';
import messages from './i18n/messages';
import daedalusTheme from './themes/daedalus';
import store from './store';

@observer
export default class App extends Component {

  static propTypes = {
    children: PropTypes.element.isRequired
  };

  render() {
    const { locale } = store.i18n;
    return (
      <IntlProvider locale={locale} messages={messages[locale]}>
        <MuiThemeProvider muiTheme={getMuiTheme(daedalusTheme)}>
          <Provider store={store}>
            {this.props.children}
          </Provider>
        </MuiThemeProvider>
      </IntlProvider>
    );
  }
}
