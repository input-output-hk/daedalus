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
var __metadata =
  (this && this.__metadata) ||
  function (k, v) {
    if (typeof Reflect === 'object' && typeof Reflect.metadata === 'function')
      return Reflect.metadata(k, v);
  };
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importStar(require('react'));
const moment_1 = __importDefault(require('moment'));
const mobx_react_1 = require('mobx-react');
const lodash_1 = require('lodash');
const react_markdown_1 = __importDefault(require('react-markdown'));
const ButtonSkin_1 = require('@react-polymorph/skins/simple/ButtonSkin');
const DialogCloseButton_1 = __importDefault(
  require('../widgets/DialogCloseButton')
);
const close_cross_thin_inline_svg_1 = __importDefault(
  require('../../assets/images/close-cross-thin.inline.svg')
);
const ButtonLink_1 = __importDefault(require('../widgets/ButtonLink'));
const AlertsOverlay_scss_1 = __importDefault(require('./AlertsOverlay.scss'));
let AlertsOverlay = class AlertsOverlay extends react_1.Component {
  constructor(props) {
    super(props);
    this.state = {
      showOverlay: true,
    };
  }
  localizedDateFormat;
  componentDidMount() {
    // @ts-ignore ts-migrate(2322) FIXME: Type 'string' is not assignable to type '"MM/DD/YY... Remove this comment to see the full error message
    this.localizedDateFormat = moment_1.default
      .localeData()
      .longDateFormat('L');
  }
  contentClickHandler(event) {
    const linkUrl = (0, lodash_1.get)(event, ['target', 'href']);
    if (linkUrl) {
      event.preventDefault();
      this.props.onOpenExternalLink(linkUrl);
    }
  }
  onClose = () => {
    const { alerts, onMarkNewsAsRead, onCloseOpenAlert } = this.props;
    if (alerts.length <= 1) {
      onMarkNewsAsRead([alerts[0].id]);
      onCloseOpenAlert();
      this.setState({
        showOverlay: false,
      });
      return;
    }
    onMarkNewsAsRead([alerts[0].id]);
  };
  onProceedNewsAction = (event) => {
    const { onProceedNewsAction, alerts } = this.props;
    onProceedNewsAction(alerts[0], event);
  };
  renderAction = (action) => {
    if (action && (action.url || action.event)) {
      return react_1.default.createElement(
        ButtonLink_1.default,
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
        {
          // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
          className: AlertsOverlay_scss_1.default.actionBtn,
          onClick: this.onProceedNewsAction,
          skin: ButtonSkin_1.ButtonSkin,
          label: action.label,
          linkProps: {
            className: AlertsOverlay_scss_1.default.externalLink,
            hasIconBefore: false,
            hasIconAfter: action.url && true,
          },
        }
      );
    }
    return null;
  };
  // @ts-ignore ts-migrate(2503) FIXME: Cannot find namespace 'News'.
  renderCounter = (alerts) => {
    const { allAlertsCount, hideCounter } = this.props;
    if (!hideCounter && allAlertsCount > 1) {
      return react_1.default.createElement(
        'span',
        { className: AlertsOverlay_scss_1.default.counter },
        allAlertsCount - alerts.length + 1,
        ' / ',
        allAlertsCount
      );
    }
    return null;
  };
  render() {
    const { showOverlay } = this.state;
    const { alerts, currentDateFormat } = this.props;
    const [alert] = alerts;
    const { content, date, action, title } = alert;
    return (
      showOverlay &&
      react_1.default.createElement(
        'div',
        { className: AlertsOverlay_scss_1.default.component },
        react_1.default.createElement(DialogCloseButton_1.default, {
          className: AlertsOverlay_scss_1.default.closeButton,
          icon: close_cross_thin_inline_svg_1.default,
          onClose: this.onClose,
        }),
        this.renderCounter(alerts),
        react_1.default.createElement(
          'h1',
          { className: AlertsOverlay_scss_1.default.title },
          title
        ),
        react_1.default.createElement(
          'span',
          { className: AlertsOverlay_scss_1.default.date },
          (0, moment_1.default)(date).format(currentDateFormat)
        ),
        react_1.default.createElement(
          'div',
          {
            className: AlertsOverlay_scss_1.default.content,
            role: 'presentation',
            onClick: this.contentClickHandler.bind(this),
          },
          react_1.default.createElement(react_markdown_1.default, {
            escapeHtml: false,
            source: content,
          })
        ),
        this.renderAction(action)
      )
    );
  }
};
AlertsOverlay = __decorate(
  [mobx_react_1.observer, __metadata('design:paramtypes', [Object])],
  AlertsOverlay
);
exports.default = AlertsOverlay;
//# sourceMappingURL=AlertsOverlay.js.map
