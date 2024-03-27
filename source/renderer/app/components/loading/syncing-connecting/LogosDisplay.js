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
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const classnames_1 = __importDefault(require('classnames'));
const react_lottie_1 = __importDefault(require('react-lottie'));
const LogosDisplay_scss_1 = __importDefault(require('./LogosDisplay.scss'));
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/ada-log... Remove this comment to see the full error message
const ada_logo_inline_svg_1 = __importDefault(
  require('../../../assets/images/ada-logo.inline.svg')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/cardano... Remove this comment to see the full error message
const cardano_logo_inline_svg_1 = __importDefault(
  require('../../../assets/images/cardano-logo.inline.svg')
);
const logo_animation_data_json_1 = __importDefault(
  require('./logo-animation-data.json')
);
const logoAnimationOptionsLottie = {
  loop: true,
  autoplay: true,
  animationData: logo_animation_data_json_1.default,
  rendererSettings: {
    preserveAspectRatio: 'xMidYMid slice',
  },
};
class LogosDisplay extends react_1.Component {
  componentDidMount() {
    // Manual adjustment due to `logo-animation-data.json` canvas size
    const svg = document.querySelector('.LogosDisplay_daedalusLogo svg');
    svg.setAttribute('viewBox', '534 250 212 220');
  }
  render() {
    const { isConnected } = this.props;
    const currencyLogoStyles = (0, classnames_1.default)([
      LogosDisplay_scss_1.default.adaLogo,
      !isConnected
        ? LogosDisplay_scss_1.default.connectingLogo
        : LogosDisplay_scss_1.default.syncingLogo,
    ]);
    const daedalusLogoStyles = (0, classnames_1.default)([
      LogosDisplay_scss_1.default.daedalusLogo,
      !isConnected
        ? LogosDisplay_scss_1.default.connectingLogo
        : LogosDisplay_scss_1.default.syncingLogo,
    ]);
    const apiLogoStyles = (0, classnames_1.default)([
      LogosDisplay_scss_1.default.adaApiLogo,
      !isConnected
        ? LogosDisplay_scss_1.default.connectingLogo
        : LogosDisplay_scss_1.default.syncingLogo,
    ]);
    return react_1.default.createElement(
      'div',
      { className: LogosDisplay_scss_1.default.component },
      react_1.default.createElement(react_svg_inline_1.default, {
        svg: ada_logo_inline_svg_1.default,
        className: currencyLogoStyles,
      }),
      react_1.default.createElement(
        'div',
        { className: daedalusLogoStyles },
        react_1.default.createElement(react_lottie_1.default, {
          options: logoAnimationOptionsLottie,
        })
      ),
      react_1.default.createElement(react_svg_inline_1.default, {
        svg: cardano_logo_inline_svg_1.default,
        className: apiLogoStyles,
      })
    );
  }
}
exports.default = LogosDisplay;
//# sourceMappingURL=LogosDisplay.js.map
