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
const mobx_react_1 = require('mobx-react');
const Navigation_scss_1 = __importDefault(require('./Navigation.scss'));
const NavButton_1 = __importDefault(require('./NavButton'));
const NavDropdown_1 = __importDefault(require('./NavDropdown'));
let Navigation = class Navigation extends react_1.Component {
  isActiveNavItem = (id, item) => {
    let result = false;
    if (!item) {
      result = id === this.props.activeItem;
    }
    return result;
  };
  render() {
    const {
      isActiveNavItem = this.isActiveNavItem,
      onNavItemClick,
      activeItem,
      items,
    } = this.props;
    return react_1.default.createElement(
      'div',
      { className: Navigation_scss_1.default.component },
      items.map(({ id, icon, label, hasNotification, ...item }) =>
        // @ts-ignore ts-migrate(2367) FIXME: This condition will always return 'false' since th... Remove this comment to see the full error message
        item.type === 'dropdown'
          ? react_1.default.createElement(NavDropdown_1.default, {
              key: id,
              label: label,
              icon: icon,
              isActive: isActiveNavItem(id, item),
              onChange: (i) => onNavItemClick(i),
              activeItem: activeItem,
              // @ts-ignore ts-migrate(2339) FIXME: Property 'options' does not exist on type '{ type?... Remove this comment to see the full error message
              options: item.options,
              hasNotification: hasNotification,
            })
          : react_1.default.createElement(NavButton_1.default, {
              key: id,
              className: id,
              label: label,
              icon: icon,
              isActive: isActiveNavItem(id, item),
              onClick: () => onNavItemClick(id),
              hasNotification: hasNotification,
            })
      )
    );
  }
};
Navigation = __decorate([mobx_react_1.observer], Navigation);
exports.default = Navigation;
//# sourceMappingURL=Navigation.js.map
