import React from 'react';
import { action, configure } from 'mobx';
import { render } from 'react-dom';
import { addLocaleData } from 'react-intl';
import en from 'react-intl/locale-data/en';
import ja from 'react-intl/locale-data/ja';
import { createHashHistory } from 'history';
import { RouterStore, syncHistoryWithStore } from 'mobx-react-router';
import App from './App';
import { setUpStores } from './stores';
import actions from './actions';
import utils from './utils';
import Action from './actions/lib/Action';
import translations from './i18n/translations';
import '!style-loader!css-loader!sass-loader!./themes/index.global.scss'; // eslint-disable-line
import { setupApi } from './api';
import LocalStorageApi from './api/utils/localStorage';
import {
  DiscreetModeFeatureProvider,
  LocalStorageFeatureProvider,
} from './features';
import { MatomoAnalyticsTracker } from './analytics/MatomoAnalyticsTracker';
import { AnalyticsProvider } from './components/analytics';
// run MobX in strict mode
configure({
  enforceActions: 'always',
});
// https://github.com/yahoo/react-intl/wiki#loading-locale-data
addLocaleData([...en, ...ja]);
// @ts-ignore ts-migrate(2339) FIXME: Property 'environment' does not exist on type 'typ... Remove this comment to see the full error message
const { environment } = global;
const { isTest } = environment;

const initializeDaedalus = () => {
  const api = setupApi(isTest);
  const hashHistory = createHashHistory();
  const routingStore = new RouterStore();
  const analyticsTracker = new MatomoAnalyticsTracker(
    environment,
    api.localStorage,
    api.ada
  );
  const stores = setUpStores(api, actions, routingStore, analyticsTracker);
  const history = syncHistoryWithStore(hashHistory, routingStore);
  // @ts-ignore ts-migrate(2339) FIXME: Property 'daedalus' does not exist on type 'Window... Remove this comment to see the full error message
  window.daedalus = {
    api,
    environment,
    actions,
    utils,
    stores,
    translations,
    reset: action(() => {
      Action.resetAllActions();
      setUpStores(api, actions, routingStore, analyticsTracker);
    }),
  };
  const rootElement = document.getElementById('root');
  if (!rootElement) throw new Error('No #root element found.');
  render(
    <LocalStorageFeatureProvider localStorage={LocalStorageApi}>
      <AnalyticsProvider tracker={analyticsTracker}>
        <DiscreetModeFeatureProvider>
          <App stores={stores} actions={actions} history={history} />
        </DiscreetModeFeatureProvider>
      </AnalyticsProvider>
    </LocalStorageFeatureProvider>,
    rootElement
  );
};

window.addEventListener('load', initializeDaedalus);
window.addEventListener('dragover', (event) => event.preventDefault());
window.addEventListener('drop', (event) => event.preventDefault());
