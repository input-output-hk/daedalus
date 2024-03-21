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
const classnames_1 = __importDefault(require('classnames'));
const moment_1 = __importDefault(require('moment'));
const UpdateItem_scss_1 = __importDefault(require('./UpdateItem.scss'));
let UpdateItem = class UpdateItem extends react_1.Component {
  static defaultProps = {
    onupdateItemActionClick: null,
    expandWithoutTransition: false,
  };
  generateTitleWithBadge = (title) => {
    const wordsArray = title.split(' ');
    const lastWordIndex = wordsArray.length - 1;
    const lastWord = wordsArray[lastWordIndex];
    // Remove last word from array
    wordsArray.splice(lastWordIndex, 1);
    // Join words without last one
    const firstSentencePart = wordsArray.join(' ');
    return react_1.default.createElement(
      'h4',
      { className: UpdateItem_scss_1.default.title },
      firstSentencePart ? `${firstSentencePart} ` : null,
      react_1.default.createElement(
        'span',
        { className: UpdateItem_scss_1.default.lastWordWrapper },
        lastWord,
        '\u00A0',
        react_1.default.createElement('span', {
          className: UpdateItem_scss_1.default.badge,
        })
      )
    );
  };
  render() {
    const {
      updateItem,
      currentDateFormat,
      onOpenAppUpdate,
      downloadProgress,
      isUpdatePostponed,
    } = this.props;
    const componentClasses = (0, classnames_1.default)([
      UpdateItem_scss_1.default.component,
      updateItem.type ? UpdateItem_scss_1.default[updateItem.type] : null,
    ]);
    const title = this.generateTitleWithBadge(updateItem.title);
    return react_1.default.createElement(
      'div',
      {
        className: componentClasses,
        role: 'presentation',
        onClick: onOpenAppUpdate,
      },
      title,
      react_1.default.createElement(
        'div',
        { className: UpdateItem_scss_1.default.date },
        (0, moment_1.default)(updateItem.date).format(currentDateFormat)
      ),
      !isUpdatePostponed &&
        react_1.default.createElement(
          'div',
          { className: UpdateItem_scss_1.default.downloadProgress },
          react_1.default.createElement('span', {
            style: {
              width: `${downloadProgress}%`,
            },
          }),
          react_1.default.createElement('em', null)
        )
    );
  }
};
UpdateItem = __decorate([mobx_react_1.observer], UpdateItem);
exports.default = UpdateItem;
//# sourceMappingURL=UpdateItem.js.map
