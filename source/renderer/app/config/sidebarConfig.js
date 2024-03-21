'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.FUZZY_SEARCH_THRESHOLD = exports.sidebarConfig = exports.CATEGORIES_LIST = exports.CATEGORIES_WITHOUT_DELEGATION_COUNTDOWN = exports.CATEGORIES_WITH_DELEGATION_COUNTDOWN = exports.CATEGORIES_BY_NAME = void 0;
const routes_config_1 = require('../routes-config');
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../assets/images/sidebar/walle... Remove this comment to see the full error message
const wallet_ic_inline_svg_1 = __importDefault(
  require('../assets/images/sidebar/wallet-ic.inline.svg')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../assets/images/sidebar/setti... Remove this comment to see the full error message
const settings_ic_inline_svg_1 = __importDefault(
  require('../assets/images/sidebar/settings-ic.inline.svg')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../assets/images/sidebar/paper... Remove this comment to see the full error message
const paper_certificate_ic_inline_svg_1 = __importDefault(
  require('../assets/images/sidebar/paper-certificate-ic.inline.svg')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../assets/images/sidebar/deleg... Remove this comment to see the full error message
const delegation_ic_inline_svg_1 = __importDefault(
  require('../assets/images/sidebar/delegation-ic.inline.svg')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../assets/images/sidebar/deleg... Remove this comment to see the full error message
const delegation_progress_ic_inline_svg_1 = __importDefault(
  require('../assets/images/sidebar/delegation-progress-ic.inline.svg')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../assets/images/sidebar/netwo... Remove this comment to see the full error message
const network_info_logo_cardano_ic_inline_svg_1 = __importDefault(
  require('../assets/images/sidebar/network-info-logo-cardano-ic.inline.svg')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../assets/images/sidebar/votin... Remove this comment to see the full error message
const voting_ic_inline_svg_1 = __importDefault(
  require('../assets/images/sidebar/voting-ic.inline.svg')
);
exports.CATEGORIES_BY_NAME = {
  WALLETS: {
    name: 'WALLETS',
    icon: wallet_ic_inline_svg_1.default,
    route: routes_config_1.ROUTES.WALLETS.ROOT,
    tooltipTextId: 'wallets',
  },
  PAPER_WALLET_CREATE_CERTIFICATE: {
    name: 'PAPER_WALLET_CREATE_CERTIFICATE',
    icon: paper_certificate_ic_inline_svg_1.default,
    route: routes_config_1.ROUTES.PAPER_WALLET_CREATE_CERTIFICATE,
  },
  STAKING_DELEGATION_COUNTDOWN: {
    name: 'STAKING_DELEGATION_COUNTDOWN',
    icon: delegation_ic_inline_svg_1.default,
    route: routes_config_1.ROUTES.STAKING.COUNTDOWN,
    tooltipTextId: 'staking',
  },
  STAKING: {
    name: 'STAKING',
    icon: delegation_progress_ic_inline_svg_1.default,
    route: routes_config_1.ROUTES.STAKING.ROOT,
    tooltipTextId: 'staking',
  },
  SETTINGS: {
    name: 'SETTINGS',
    icon: settings_ic_inline_svg_1.default,
    route: routes_config_1.ROUTES.SETTINGS.ROOT,
    tooltipTextId: 'settings',
  },
  NETWORK_INFO: {
    name: 'NETWORK_INFO',
    icon: network_info_logo_cardano_ic_inline_svg_1.default,
    route: routes_config_1.ROUTES.NETWORK_INFO,
  },
  VOTING: {
    name: 'VOTING',
    icon: voting_ic_inline_svg_1.default,
    route: routes_config_1.ROUTES.VOTING.REGISTRATION,
    tooltipTextId: 'voting',
  },
};
exports.CATEGORIES_WITH_DELEGATION_COUNTDOWN = [
  exports.CATEGORIES_BY_NAME.WALLETS,
  exports.CATEGORIES_BY_NAME.STAKING_DELEGATION_COUNTDOWN,
  exports.CATEGORIES_BY_NAME.SETTINGS,
  exports.CATEGORIES_BY_NAME.NETWORK_INFO,
];
exports.CATEGORIES_WITHOUT_DELEGATION_COUNTDOWN = [
  exports.CATEGORIES_BY_NAME.WALLETS,
  exports.CATEGORIES_BY_NAME.STAKING,
  exports.CATEGORIES_BY_NAME.SETTINGS,
  exports.CATEGORIES_BY_NAME.NETWORK_INFO,
];
exports.CATEGORIES_LIST = [
  exports.CATEGORIES_BY_NAME.WALLETS,
  exports.CATEGORIES_BY_NAME.STAKING_DELEGATION_COUNTDOWN,
  exports.CATEGORIES_BY_NAME.STAKING,
  exports.CATEGORIES_BY_NAME.VOTING,
  exports.CATEGORIES_BY_NAME.SETTINGS,
  exports.CATEGORIES_BY_NAME.NETWORK_INFO,
];
exports.sidebarConfig = {
  CATEGORIES_BY_NAME: exports.CATEGORIES_BY_NAME,
  CATEGORIES_LIST: exports.CATEGORIES_LIST,
};
exports.FUZZY_SEARCH_THRESHOLD = 0.5;
//# sourceMappingURL=sidebarConfig.js.map
