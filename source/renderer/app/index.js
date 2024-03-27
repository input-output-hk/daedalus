'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importDefault(require('react'));
const mobx_1 = require('mobx');
const react_dom_1 = require('react-dom');
const react_intl_1 = require('react-intl');
const en_1 = __importDefault(require('react-intl/locale-data/en'));
const ja_1 = __importDefault(require('react-intl/locale-data/ja'));
const history_1 = require('history');
const mobx_react_router_1 = require('mobx-react-router');
const App_1 = __importDefault(require('./App'));
const stores_1 = require('./stores');
const actions_1 = __importDefault(require('./actions'));
const utils_1 = __importDefault(require('./utils'));
const Action_1 = __importDefault(require('./actions/lib/Action'));
const translations_1 = __importDefault(require('./i18n/translations'));
require('!style-loader!css-loader!sass-loader!./themes/index.global.scss'); // eslint-disable-line
const api_1 = require('./api');
const localStorage_1 = __importDefault(require('./api/utils/localStorage'));
const features_1 = require('./features');
const MatomoAnalyticsTracker_1 = require('./analytics/MatomoAnalyticsTracker');
const analytics_1 = require('./components/analytics');
// run MobX in strict mode
(0, mobx_1.configure)({
  enforceActions: 'always',
});
// https://github.com/yahoo/react-intl/wiki#loading-locale-data
(0, react_intl_1.addLocaleData)([...en_1.default, ...ja_1.default]);
// @ts-ignore ts-migrate(2339) FIXME: Property 'environment' does not exist on type 'typ... Remove this comment to see the full error message
const { environment } = global;
const { isTest } = environment;
const initializeDaedalus = () => {
  const api = (0, api_1.setupApi)(isTest);
  const hashHistory = (0, history_1.createHashHistory)();
  const routingStore = new mobx_react_router_1.RouterStore();
  const analyticsTracker = new MatomoAnalyticsTracker_1.MatomoAnalyticsTracker(
    environment,
    api.localStorage,
    api.ada
  );
  const stores = (0, stores_1.setUpStores)(
    api,
    actions_1.default,
    routingStore,
    analyticsTracker
  );
  const history = (0, mobx_react_router_1.syncHistoryWithStore)(
    hashHistory,
    routingStore
  );
  // @ts-ignore ts-migrate(2339) FIXME: Property 'daedalus' does not exist on type 'Window... Remove this comment to see the full error message
  window.daedalus = {
    api,
    environment,
    actions: actions_1.default,
    utils: utils_1.default,
    stores,
    translations: translations_1.default,
    reset: (0, mobx_1.action)(() => {
      Action_1.default.resetAllActions();
      (0, stores_1.setUpStores)(
        api,
        actions_1.default,
        routingStore,
        analyticsTracker
      );
    }),
  };
  const rootElement = document.getElementById('root');
  if (!rootElement) throw new Error('No #root element found.');
  (0, react_dom_1.render)(
    react_1.default.createElement(
      features_1.LocalStorageFeatureProvider,
      { localStorage: localStorage_1.default },
      react_1.default.createElement(
        analytics_1.AnalyticsProvider,
        { tracker: analyticsTracker },
        react_1.default.createElement(
          features_1.DiscreetModeFeatureProvider,
          null,
          react_1.default.createElement(App_1.default, {
            stores: stores,
            actions: actions_1.default,
            history: history,
          })
        )
      )
    ),
    rootElement
  );
};
window.addEventListener('load', initializeDaedalus);
window.addEventListener('dragover', (event) => event.preventDefault());
window.addEventListener('drop', (event) => event.preventDefault());
//# sourceMappingURL=index.js.map
