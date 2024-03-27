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
const react_intl_1 = require('react-intl');
const Link_1 = require('@react-polymorph/components/Link');
const LinkSkin_1 = require('@react-polymorph/skins/simple/LinkSkin');
const DialogCloseButton_1 = __importDefault(
  require('../widgets/DialogCloseButton')
);
const global_messages_1 = __importDefault(
  require('../../i18n/global-messages')
);
const About_scss_1 = __importDefault(require('./About.scss'));
const close_cross_thin_inline_svg_1 = __importDefault(
  require('../../assets/images/close-cross-thin.inline.svg')
);
const daedalus_logo_loading_grey_inline_svg_1 = __importDefault(
  require('../../assets/images/daedalus-logo-loading-grey.inline.svg')
);
const cardano_logo_inline_svg_1 = __importDefault(
  require('../../assets/images/cardano-logo.inline.svg')
);
const messages = (0, react_intl_1.defineMessages)({
  aboutTitle: {
    id: 'static.about.title',
    defaultMessage: '!!!Daedalus',
    description: 'About "title"',
  },
  aboutContentDaedalusHeadline: {
    id: 'static.about.content.daedalus.headline',
    defaultMessage: '!!!Daedalus Team:',
    description: 'About page daedalus team headline',
  },
  aboutContentCardanoHeadline: {
    id: 'static.about.content.cardano.headline',
    defaultMessage: '!!!Cardano Team:',
    description: 'About page cardano team headline',
  },
  aboutContentDaedalusMembers: {
    id: 'static.about.content.daedalus.members',
    defaultMessage:
      '!!!Alan McNicholas, Aleksandar Djordjevic, Alexander Rukin, Brian McKenna, Charles Hoskinson, Daniel Main, Danilo Prates, Darko Mijić, Dmitrii Gaico, Dominik Guzei, Elin Liu, Gabriela Ponce, Jane Wild, Jeremy Wood, Juli Sudi, Junko Oda, Laurie Wang, Lucas Araujo, Manus McCole, Marcin Mazurek, Michael Bishop, Michael Chappell, Michal Rus, Mior Sufian, Nikola Glumac, Piotr Stachyra, Przemysław Włodek, Renan Ferreira, Rhys Bartels-Waller, Richard Wild, Robert Moore, Rodney Lorrimar, Sam Jeston, Samuel Leathers, Serge Kosyrev, Szymon Masłowski, Tatyana Valkevych, Tomas Vrana, Tomislav Horaček, Yakov Karavelov',
    description: 'About page daedalus team members',
  },
  aboutContentCardanoMembers: {
    id: 'static.about.content.cardano.members',
    defaultMessage:
      '!!!Alan McNicholas, Alejandro Garcia, Alexander Diemand, Alexander Vieth, Anatoli Ivanou, Andreas Triantafyllos, Ante Kegalj, Armando Santos, Ben Ford, Charles Hoskinson, Dan Friedman, Deepak Kapiswe, Denis Shevchenko, Dorin Solomon, Duncan Coutts, Edsko de Vries, Erik de Castro Lopo, Gerard Moroney, Heinrich Apfelmus, Hiroto Shioi, Jane Wild, Jean-Christophe Mincke, Jeremy Wood, Johannes Lund, Jonathan Knowles, Jordan Millar, Karl Knutsson, Kristijan Šarić, Lars Brünjes, Laurie Wang, Liz Bancroft, Luke Nadur, Marc Fontaine, Marcin Szamotulski, Matt Parsons, Matthias Benkort, Michael Bishop, Michael Hueschen, Moritz Angermann, Neil Davis, Niamh Ahern, Nicholas Clarke, Nicolas Di Prima, Noel Rimbert, Paolo Veronelli, Patrick Kelly, Pawel Jakubas, Peter Gaži, Peter Thompson, Philipp Kant, Piotr Stachyra, Ravi Patel, Richard Wild, Rob Cohen, Rodney Lorrimar, Ryan Lemmer, Samuel Leathers, Serge Kosyrev, Tatyana Valkevych, Tom Flynn, Vasileios Gkoumas, Vincent Hanquez, Yuriy Lazaryev',
    description: 'About page cardano team members',
  },
  aboutCopyright: {
    id: 'static.about.copyright',
    defaultMessage: '!!!Input Output HK Limited. Licensed under',
    description: 'About "copyright"',
  },
  licenseLink: {
    id: 'static.about.license',
    defaultMessage: '!!!Apache 2.0 license',
    description: 'About page license name',
  },
  aboutBuildInfo: {
    id: 'static.about.buildInfo',
    defaultMessage: '!!!MacOS build 3769, with Cardano 1.0.4',
    description: 'About page build information',
  },
});
class About extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  render() {
    const { intl } = this.context;
    const {
      apiVersion,
      nodeVersion,
      build,
      onOpenExternalLink,
      os,
      version,
      onClose,
    } = this.props;
    const apiName = intl.formatMessage(global_messages_1.default.apiName);
    const apiIcon = cardano_logo_inline_svg_1.default;
    const apiHeadline = intl.formatMessage(
      messages.aboutContentCardanoHeadline
    );
    const apiMembers = intl.formatMessage(messages.aboutContentCardanoMembers);
    return react_1.default.createElement(
      'div',
      { className: About_scss_1.default.container },
      react_1.default.createElement(DialogCloseButton_1.default, {
        className: About_scss_1.default.closeButton,
        icon: close_cross_thin_inline_svg_1.default,
        onClose: onClose,
      }),
      react_1.default.createElement(
        'div',
        { className: About_scss_1.default.headerWrapper },
        react_1.default.createElement(react_svg_inline_1.default, {
          svg: daedalus_logo_loading_grey_inline_svg_1.default,
          className: About_scss_1.default.daedalusIcon,
        }),
        react_1.default.createElement(
          'div',
          { className: About_scss_1.default.daedalusTitleVersion },
          react_1.default.createElement(
            'div',
            { className: About_scss_1.default.daedalusTitle },
            intl.formatMessage(messages.aboutTitle),
            react_1.default.createElement(
              'span',
              { className: About_scss_1.default.daedalusVersion },
              version
            )
          ),
          react_1.default.createElement(
            'div',
            { className: About_scss_1.default.daedalusBuildInfo },
            react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
              ...messages.aboutBuildInfo,
              values: {
                platform: os,
                build,
                apiName,
                apiVersion,
                nodeVersion,
              },
            })
          )
        ),
        react_1.default.createElement(react_svg_inline_1.default, {
          svg: apiIcon,
          className: About_scss_1.default.apiIcon,
        })
      ),
      react_1.default.createElement(
        'div',
        { className: About_scss_1.default.contentText },
        react_1.default.createElement(
          'h2',
          null,
          intl.formatMessage(messages.aboutContentDaedalusHeadline)
        ),
        react_1.default.createElement(
          'div',
          { className: About_scss_1.default.contentDaedalus },
          intl.formatMessage(messages.aboutContentDaedalusMembers)
        ),
        react_1.default.createElement('h2', null, apiHeadline),
        react_1.default.createElement('div', null, apiMembers)
      ),
      react_1.default.createElement(
        'div',
        { className: About_scss_1.default.footerWrapper },
        react_1.default.createElement(Link_1.Link, {
          className: About_scss_1.default.link,
          onClick: () => onOpenExternalLink('https://daedaluswallet.io'),
          label: 'http://daedaluswallet.io',
          skin: LinkSkin_1.LinkSkin,
        }),
        react_1.default.createElement(
          'div',
          { className: About_scss_1.default.copyright },
          intl.formatMessage(messages.aboutCopyright),
          '\u00A0',
          react_1.default.createElement(Link_1.Link, {
            className: About_scss_1.default.link,
            onClick: () =>
              onOpenExternalLink(
                'https://github.com/input-output-hk/daedalus/blob/master/LICENSE'
              ),
            label: intl.formatMessage(messages.licenseLink),
            skin: LinkSkin_1.LinkSkin,
          })
        )
      )
    );
  }
}
exports.default = About;
//# sourceMappingURL=About.js.map
