'use strict';
var __createBinding =
  (this && this.__createBinding) ||
  (Object.create
    ? function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        var desc = Object.getOwnPropertyDescriptor(m, k);
        if (
          !desc ||
          ('get' in desc ? !m.__esModule : desc.writable || desc.configurable)
        ) {
          desc = {
            enumerable: true,
            get: function () {
              return m[k];
            },
          };
        }
        Object.defineProperty(o, k2, desc);
      }
    : function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        o[k2] = m[k];
      });
var __setModuleDefault =
  (this && this.__setModuleDefault) ||
  (Object.create
    ? function (o, v) {
        Object.defineProperty(o, 'default', { enumerable: true, value: v });
      }
    : function (o, v) {
        o['default'] = v;
      });
var __decorate =
  (this && this.__decorate) ||
  function (decorators, target, key, desc) {
    var c = arguments.length,
      r =
        c < 3
          ? target
          : desc === null
          ? (desc = Object.getOwnPropertyDescriptor(target, key))
          : desc,
      d;
    if (typeof Reflect === 'object' && typeof Reflect.decorate === 'function')
      r = Reflect.decorate(decorators, target, key, desc);
    else
      for (var i = decorators.length - 1; i >= 0; i--)
        if ((d = decorators[i]))
          r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
  };
var __importStar =
  (this && this.__importStar) ||
  function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null)
      for (var k in mod)
        if (k !== 'default' && Object.prototype.hasOwnProperty.call(mod, k))
          __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
  };
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importStar(require('react'));
const classnames_1 = __importDefault(require('classnames'));
const mobx_react_1 = require('mobx-react');
const react_intl_1 = require('react-intl');
const DisplaySettings_scss_1 = __importDefault(
  require('./DisplaySettings.scss')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/themes/... Remove this comment to see the full error message
const incentivized_testnet_png_1 = __importDefault(
  require('../../../assets/images/themes/incentivized-testnet.png')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/themes/... Remove this comment to see the full error message
const cardano_png_1 = __importDefault(
  require('../../../assets/images/themes/cardano.png')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/themes/... Remove this comment to see the full error message
const dark_blue_png_1 = __importDefault(
  require('../../../assets/images/themes/dark-blue.png')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/themes/... Remove this comment to see the full error message
const dark_cardano_png_1 = __importDefault(
  require('../../../assets/images/themes/dark-cardano.png')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/themes/... Remove this comment to see the full error message
const flight_candidate_png_1 = __importDefault(
  require('../../../assets/images/themes/flight-candidate.png')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/themes/... Remove this comment to see the full error message
const light_blue_png_1 = __importDefault(
  require('../../../assets/images/themes/light-blue.png')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/themes/... Remove this comment to see the full error message
const shelley_testnet_png_1 = __importDefault(
  require('../../../assets/images/themes/shelley-testnet.png')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/themes/... Remove this comment to see the full error message
const yellow_png_1 = __importDefault(
  require('../../../assets/images/themes/yellow.png')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/themes/... Remove this comment to see the full error message
const white_png_1 = __importDefault(
  require('../../../assets/images/themes/white.png')
);
const index_1 = require('../../../themes/index');
const messages = (0, react_intl_1.defineMessages)({
  themeLabel: {
    id: 'settings.display.themeLabel',
    defaultMessage: '!!!Theme',
    description:
      'Label for the "Theme" selection on the display settings page.',
  },
  themeIncentivizedTestnet: {
    id: 'settings.display.themeNames.incentivizedTestnet',
    defaultMessage: '!!!Incentivized Testnet',
    description:
      'Name of the "Incentivized Testnet" theme on the display settings page.',
  },
  themeLightBlue: {
    id: 'settings.display.themeNames.lightBlue',
    defaultMessage: '!!!Light blue',
    description: 'Name of the "Light blue" theme on the display settings page.',
  },
  themeCardano: {
    id: 'settings.display.themeNames.cardano',
    defaultMessage: '!!!Cardano',
    description: 'Name of the "Cardano" theme on the display settings page.',
  },
  themeDarkBlue: {
    id: 'settings.display.themeNames.darkBlue',
    defaultMessage: '!!!Dark blue',
    description: 'Name of the "Dark blue" theme on the display settings page.',
  },
  themeDarkCardano: {
    id: 'settings.display.themeNames.darkCardano',
    defaultMessage: '!!!Dark Cardano',
    description:
      'Name of the "Dark cardano" theme on the display settings page.',
  },
  themeFlightCandidate: {
    id: 'settings.display.themeNames.flightCandidate',
    defaultMessage: '!!!Flight Candidate',
    description:
      'Name of the "Flight Candidate" theme on the display settings page.',
  },
  themeShelleyTestnet: {
    id: 'settings.display.themeNames.shelleyTestnet',
    defaultMessage: '!!!Shelley Testnet',
    description:
      'Name of the "Shelley Testnet" theme on the display settings page.',
  },
  themeYellow: {
    id: 'settings.display.themeNames.yellow',
    defaultMessage: '!!!Yellow',
    description: 'Name of the "Yellow" theme on the display settings page.',
  },
  themeWhite: {
    id: 'settings.display.themeNames.white',
    defaultMessage: '!!!White',
    description: 'Name of the "White" theme on the display settings page.',
  },
});
let DisplaySettings = class DisplaySettings extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  render() {
    const { theme, selectTheme } = this.props;
    const { intl } = this.context;
    // @ts-ignore ts-migrate(2339) FIXME: Property 'isFlight' does not exist on type 'typeof... Remove this comment to see the full error message
    const { isFlight, environment } = global;
    const { isDev } = environment;
    const themeIncentivizedTestnetClasses = (0, classnames_1.default)([
      theme === index_1.THEMES.INCENTIVIZED_TESTNET
        ? DisplaySettings_scss_1.default.active
        : DisplaySettings_scss_1.default.inactive,
      DisplaySettings_scss_1.default.themeImageWrapper,
    ]);
    const themeLightBlueClasses = (0, classnames_1.default)([
      theme === index_1.THEMES.LIGHT_BLUE
        ? DisplaySettings_scss_1.default.active
        : DisplaySettings_scss_1.default.inactive,
      DisplaySettings_scss_1.default.themeImageWrapper,
    ]);
    const themeCardanoClasses = (0, classnames_1.default)([
      theme === index_1.THEMES.CARDANO
        ? DisplaySettings_scss_1.default.active
        : DisplaySettings_scss_1.default.inactive,
      DisplaySettings_scss_1.default.themeImageWrapper,
    ]);
    const themeDarkBlueClasses = (0, classnames_1.default)([
      theme === index_1.THEMES.DARK_BLUE
        ? DisplaySettings_scss_1.default.active
        : DisplaySettings_scss_1.default.inactive,
      DisplaySettings_scss_1.default.themeImageWrapper,
    ]);
    const themeDarkCardanoClasses = (0, classnames_1.default)([
      theme === index_1.THEMES.DARK_CARDANO
        ? DisplaySettings_scss_1.default.active
        : DisplaySettings_scss_1.default.inactive,
      DisplaySettings_scss_1.default.themeImageWrapper,
    ]);
    const themeFlightCandidateClasses = (0, classnames_1.default)([
      theme === index_1.THEMES.FLIGHT_CANDIDATE
        ? DisplaySettings_scss_1.default.active
        : DisplaySettings_scss_1.default.inactive,
      DisplaySettings_scss_1.default.themeImageWrapper,
    ]);
    const themeShelleyTestnetClasses = (0, classnames_1.default)([
      theme === index_1.THEMES.SHELLEY_TESTNET
        ? DisplaySettings_scss_1.default.active
        : DisplaySettings_scss_1.default.inactive,
      DisplaySettings_scss_1.default.themeImageWrapper,
    ]);
    const themeYellowClasses = (0, classnames_1.default)([
      theme === index_1.THEMES.YELLOW
        ? DisplaySettings_scss_1.default.active
        : DisplaySettings_scss_1.default.inactive,
      DisplaySettings_scss_1.default.themeImageWrapper,
    ]);
    const themeWhiteClasses = (0, classnames_1.default)([
      theme === index_1.THEMES.WHITE
        ? DisplaySettings_scss_1.default.active
        : DisplaySettings_scss_1.default.inactive,
      DisplaySettings_scss_1.default.themeImageWrapper,
    ]);
    return react_1.default.createElement(
      'div',
      { className: DisplaySettings_scss_1.default.component },
      react_1.default.createElement(
        'div',
        { className: DisplaySettings_scss_1.default.label },
        intl.formatMessage(messages.themeLabel)
      ),
      react_1.default.createElement(
        'div',
        { className: DisplaySettings_scss_1.default.themesRowWrapper },
        react_1.default.createElement(
          'button',
          {
            className: themeLightBlueClasses,
            onClick: selectTheme.bind(this, {
              theme: index_1.THEMES.LIGHT_BLUE,
            }),
          },
          react_1.default.createElement('img', {
            src: light_blue_png_1.default,
            role: 'presentation',
            draggable: 'false',
          }),
          react_1.default.createElement(
            'span',
            null,
            intl.formatMessage(messages.themeLightBlue)
          )
        ),
        react_1.default.createElement(
          'button',
          {
            className: themeCardanoClasses,
            onClick: selectTheme.bind(this, {
              theme: index_1.THEMES.CARDANO,
            }),
          },
          react_1.default.createElement('img', {
            src: cardano_png_1.default,
            role: 'presentation',
            draggable: 'false',
          }),
          react_1.default.createElement(
            'span',
            null,
            intl.formatMessage(messages.themeCardano)
          )
        ),
        react_1.default.createElement(
          'button',
          {
            className: themeWhiteClasses,
            onClick: selectTheme.bind(this, {
              theme: index_1.THEMES.WHITE,
            }),
          },
          react_1.default.createElement('img', {
            src: white_png_1.default,
            role: 'presentation',
            draggable: 'false',
          }),
          react_1.default.createElement(
            'span',
            null,
            intl.formatMessage(messages.themeWhite)
          )
        )
      ),
      react_1.default.createElement(
        'div',
        { className: DisplaySettings_scss_1.default.themesRowWrapper },
        react_1.default.createElement(
          'button',
          {
            className: themeDarkBlueClasses,
            onClick: selectTheme.bind(this, {
              theme: index_1.THEMES.DARK_BLUE,
            }),
          },
          react_1.default.createElement('img', {
            src: dark_blue_png_1.default,
            role: 'presentation',
            draggable: 'false',
          }),
          react_1.default.createElement(
            'span',
            null,
            intl.formatMessage(messages.themeDarkBlue)
          )
        ),
        react_1.default.createElement(
          'button',
          {
            className: themeDarkCardanoClasses,
            onClick: selectTheme.bind(this, {
              theme: index_1.THEMES.DARK_CARDANO,
            }),
          },
          react_1.default.createElement('img', {
            src: dark_cardano_png_1.default,
            role: 'presentation',
            draggable: 'false',
          }),
          react_1.default.createElement(
            'span',
            null,
            intl.formatMessage(messages.themeDarkCardano)
          )
        ),
        react_1.default.createElement(
          'button',
          {
            className: themeYellowClasses,
            onClick: selectTheme.bind(this, {
              theme: index_1.THEMES.YELLOW,
            }),
          },
          react_1.default.createElement('img', {
            src: yellow_png_1.default,
            role: 'presentation',
            draggable: 'false',
          }),
          react_1.default.createElement(
            'span',
            null,
            intl.formatMessage(messages.themeYellow)
          )
        )
      ),
      react_1.default.createElement(
        'div',
        { className: DisplaySettings_scss_1.default.themesRowWrapper },
        isDev &&
          react_1.default.createElement(
            'button',
            {
              className: themeIncentivizedTestnetClasses,
              onClick: selectTheme.bind(this, {
                theme: index_1.THEMES.INCENTIVIZED_TESTNET,
              }),
            },
            react_1.default.createElement('img', {
              src: incentivized_testnet_png_1.default,
              role: 'presentation',
              draggable: 'false',
            }),
            react_1.default.createElement(
              'span',
              null,
              intl.formatMessage(messages.themeIncentivizedTestnet)
            )
          ),
        (isDev || isFlight) &&
          react_1.default.createElement(
            'button',
            {
              className: themeFlightCandidateClasses,
              onClick: selectTheme.bind(this, {
                theme: index_1.THEMES.FLIGHT_CANDIDATE,
              }),
            },
            react_1.default.createElement('img', {
              src: flight_candidate_png_1.default,
              role: 'presentation',
              draggable: 'false',
            }),
            react_1.default.createElement(
              'span',
              null,
              intl.formatMessage(messages.themeFlightCandidate)
            )
          ),
        isDev &&
          react_1.default.createElement(
            'button',
            {
              className: themeShelleyTestnetClasses,
              onClick: selectTheme.bind(this, {
                theme: index_1.THEMES.SHELLEY_TESTNET,
              }),
            },
            react_1.default.createElement('img', {
              src: shelley_testnet_png_1.default,
              role: 'presentation',
              draggable: 'false',
            }),
            react_1.default.createElement(
              'span',
              null,
              intl.formatMessage(messages.themeShelleyTestnet)
            )
          )
      )
    );
  }
};
DisplaySettings = __decorate([mobx_react_1.observer], DisplaySettings);
exports.default = DisplaySettings;
//# sourceMappingURL=DisplaySettings.js.map
