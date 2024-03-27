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
// @ts-nocheck
const react_1 = __importStar(require('react'));
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const Button_1 = require('@react-polymorph/components/Button');
const ButtonSkin_1 = require('@react-polymorph/skins/simple/ButtonSkin');
const Link_1 = require('@react-polymorph/components/Link');
const LinkSkin_1 = require('@react-polymorph/skins/simple/LinkSkin');
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/daedalu... Remove this comment to see the full error message
const daedalus_logo_loading_grey_inline_svg_1 = __importDefault(
  require('../../../assets/images/daedalus-logo-loading-grey.inline.svg')
);
const Splash_scss_1 = __importDefault(require('./Splash.scss'));
class SplashNetwork extends react_1.Component {
  render() {
    const {
      onButtonClick,
      onLinkClick,
      title,
      subTitle1,
      subTitle2,
      description,
      buttonLabel,
      linkLabel,
      backgroundImage,
    } = this.props;
    return react_1.default.createElement(
      'div',
      { className: Splash_scss_1.default.component },
      react_1.default.createElement(
        'div',
        { className: Splash_scss_1.default.backgroundContainer },
        backgroundImage &&
          react_1.default.createElement(
            react_1.default.Fragment,
            null,
            react_1.default.createElement('div', {
              className: Splash_scss_1.default.backgroundOverlay,
            }),
            react_1.default.createElement(react_svg_inline_1.default, {
              svg: backgroundImage,
              className: Splash_scss_1.default.backgroundImage,
            })
          )
      ),
      react_1.default.createElement(
        'div',
        { className: Splash_scss_1.default.content },
        react_1.default.createElement(react_svg_inline_1.default, {
          svg: daedalus_logo_loading_grey_inline_svg_1.default,
          className: Splash_scss_1.default.daedalusIcon,
        }),
        react_1.default.createElement(
          'div',
          { className: Splash_scss_1.default.title },
          title
        ),
        react_1.default.createElement(
          'div',
          { className: Splash_scss_1.default.subTitle1 },
          subTitle1
        ),
        subTitle2 &&
          react_1.default.createElement(
            'div',
            { className: Splash_scss_1.default.subTitle2 },
            subTitle2
          ),
        react_1.default.createElement(
          'div',
          { className: Splash_scss_1.default.description },
          description
        ),
        react_1.default.createElement(
          'div',
          { className: Splash_scss_1.default.action },
          react_1.default.createElement(Button_1.Button, {
            className: Splash_scss_1.default.actionButton,
            label: buttonLabel,
            onClick: onButtonClick,
            skin: ButtonSkin_1.ButtonSkin,
          })
        ),
        linkLabel &&
          react_1.default.createElement(Link_1.Link, {
            className: Splash_scss_1.default.learnMoreLink,
            onClick: onLinkClick,
            label: linkLabel,
            skin: LinkSkin_1.LinkSkin,
          })
      )
    );
  }
}
exports.default = SplashNetwork;
//# sourceMappingURL=Splash.js.map
