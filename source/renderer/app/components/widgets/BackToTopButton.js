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
const classnames_1 = __importDefault(require('classnames'));
const react_intl_1 = require('react-intl');
const lodash_1 = require('lodash');
const BackToTopButton_scss_1 = __importDefault(
  require('./BackToTopButton.scss')
);
const messages = (0, react_intl_1.defineMessages)({
  backToTopLabel: {
    id: 'backToTopButton.label',
    defaultMessage: '!!!Back to top',
    description: '"backToTop" button label.',
  },
});
class BackToTopButton extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  static defaultProps = {
    scrollTopToActivate: 20,
    buttonTopPosition: 20,
    isForceHidden: false,
  };
  state = {
    isActive: false,
  };
  _isMounted = false;
  scrollableDomElement = null;
  componentDidMount() {
    this._isMounted = true;
    setTimeout(() => {
      if (this._isMounted) {
        this.scrollableDomElement = document.querySelector(
          `.${this.props.scrollableElementClassName}`
        );
        if (!this.scrollableDomElement) return false;
        return this.scrollableDomElement.addEventListener(
          'scroll',
          (0, lodash_1.throttle)(this.getIsBackToTopActive, 300, {
            leading: false,
            trailing: true,
          })
        );
      }
      return null;
    }, 0);
  }
  componentWillUnmount() {
    if (this._isMounted) {
      this._isMounted = false;
      this.scrollableDomElement = document.querySelector(
        `.${this.props.scrollableElementClassName}`
      );
      if (!this.scrollableDomElement) return false;
      return this.scrollableDomElement.removeEventListener(
        'scroll',
        this.getIsBackToTopActive
      );
    }
    return null;
  }
  getIsBackToTopActive = () => {
    const { isActive } = this.state;
    const { scrollTopToActivate } = this.props;
    if (this.scrollableDomElement instanceof HTMLElement && this._isMounted) {
      const scrollPosition = this.scrollableDomElement.scrollTop;
      if (scrollPosition > scrollTopToActivate && !isActive) {
        this.setState({
          isActive: true,
        });
      } else if (scrollPosition <= scrollTopToActivate && isActive) {
        this.setState({
          isActive: false,
        });
      }
    }
  };
  backToTop = () => {
    if (this.scrollableDomElement instanceof HTMLElement) {
      this.scrollableDomElement.scrollTop = 0;
    }
  };
  render() {
    const { intl } = this.context;
    const { isActive } = this.state;
    const { buttonTopPosition, isForceHidden } = this.props;
    const componentStyles = (0, classnames_1.default)(
      BackToTopButton_scss_1.default.component,
      {
        [BackToTopButton_scss_1.default.isActive]: isActive,
      }
    );
    const top = isActive ? buttonTopPosition : buttonTopPosition - 10;
    if (isForceHidden) return null;
    return react_1.default.createElement(
      'button',
      {
        style: {
          top,
        },
        className: componentStyles,
        onClick: this.backToTop,
      },
      intl.formatMessage(messages.backToTopLabel)
    );
  }
}
exports.default = BackToTopButton;
//# sourceMappingURL=BackToTopButton.js.map
