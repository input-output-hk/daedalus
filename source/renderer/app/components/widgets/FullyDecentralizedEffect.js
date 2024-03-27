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
const classnames_1 = __importDefault(require('classnames'));
const mobx_react_1 = require('mobx-react');
const fireworks_js_1 = require('fireworks-js');
const FullyDecentralizedEffect_scss_1 = __importDefault(
  require('./FullyDecentralizedEffect.scss')
);
let FullyDecentralizedEffect = class FullyDecentralizedEffect extends react_1.Component {
  constructor(props) {
    super(props);
    this.container = (0, react_1.createRef)();
  }
  container;
  fireworks = null;
  componentDidMount() {
    const { isActive } = this.props;
    const container = this.container?.current;
    if (container instanceof HTMLElement) {
      const fireworks = new fireworks_js_1.Fireworks({
        target: container,
        hue: 120,
        startDelay: 1,
        minDelay: 20,
        maxDelay: 30,
        speed: 4,
        acceleration: 1.05,
        friction: 0.98,
        gravity: 1,
        particles: 75,
        trace: 3,
        explosion: 5,
        boundaries: {
          top: 50,
          bottom: container.clientHeight,
          left: 50,
          right: container.clientWidth,
        },
      });
      this.fireworks = fireworks;
      if (isActive) {
        fireworks.start();
      }
    }
  }
  componentDidUpdate() {
    const { isActive } = this.props;
    const { fireworks } = this;
    if (isActive && fireworks) {
      fireworks.start();
    } else if (!isActive && fireworks) {
      fireworks.stop();
    }
  }
  render() {
    const { className } = this.props;
    const componentStyles = (0, classnames_1.default)([
      FullyDecentralizedEffect_scss_1.default.component,
      className,
    ]);
    return react_1.default.createElement('div', {
      className: componentStyles,
      ref: this.container,
    });
  }
};
FullyDecentralizedEffect = __decorate(
  [mobx_react_1.observer, __metadata('design:paramtypes', [Object])],
  FullyDecentralizedEffect
);
exports.default = FullyDecentralizedEffect;
//# sourceMappingURL=FullyDecentralizedEffect.js.map
