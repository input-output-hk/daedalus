// @flow
import React, { Component, PropTypes } from 'react';
import { Match, Redirect } from 'react-router';
import { Provider, observer } from 'mobx-react';
import { IntlProvider } from 'react-intl';
import { ThemeProvider } from 'react-css-themr';
import { daedalusTheme } from './themes/daedalus';
import AppStore from './stores/AppStore';
import AppController from './controllers/AppController';
import Wallet from './containers/wallet/Wallet';
import translations from './i18n/translations';

@observer
export default class App extends Component {

  static propTypes = {
    store: PropTypes.instanceOf(AppStore),
    controller: PropTypes.instanceOf(AppController),
  };

  static contextTypes = {
    router: PropTypes.object.isRequired,
  };

  render() {
    const { store, controller } = this.props;
    const { router } = this.context;
    store.setRouter(router);
    const locale = store.i18n.locale;
    return (
      <IntlProvider {...{ locale, key: locale, messages: translations[locale] }}>
        <ThemeProvider theme={daedalusTheme}>
          <Provider store={store} controller={controller}>
            <div>
              <Match pattern="/" exactly render={() => <Redirect to="/wallet" />} />
              {/* TODO: Remove redirect after main navigation is implemented */}
              <Match pattern="/wallet" component={Wallet} />
            </div>
          </Provider>
        </ThemeProvider>
      </IntlProvider>
    );
  }
}
