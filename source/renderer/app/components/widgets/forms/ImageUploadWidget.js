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
exports.messages = void 0;
const react_1 = __importStar(require('react'));
const mobx_react_1 = require('mobx-react');
const react_intl_1 = require('react-intl');
const ImageUploadWidget_scss_1 = __importDefault(
  require('./ImageUploadWidget.scss')
);
exports.messages = (0, react_intl_1.defineMessages)({
  dropFileHere: {
    id: 'ImageUploadWidget.dropFileHint',
    defaultMessage: '!!!Drop file here',
    description: 'Label "Drop file here" on the file upload widget.',
  },
  orClickToUpload: {
    id: 'ImageUploadWidget.clickToUploadLabel',
    defaultMessage: '!!!or click to upload',
    description: 'Label "or click to upload" on the file upload widget.',
  },
});
let ImageUploadWidget = class ImageUploadWidget extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  render() {
    const { intl } = this.context;
    const { label } = this.props;
    return react_1.default.createElement(
      'div',
      null,
      react_1.default.createElement(
        'div',
        { className: ImageUploadWidget_scss_1.default.label },
        label
      ),
      react_1.default.createElement(
        'div',
        { className: ImageUploadWidget_scss_1.default.uploadBox },
        react_1.default.createElement(
          'div',
          { className: ImageUploadWidget_scss_1.default.instructions },
          react_1.default.createElement(
            'div',
            { className: ImageUploadWidget_scss_1.default.title },
            intl.formatMessage(exports.messages.dropFileHere)
          ),
          react_1.default.createElement(
            'div',
            { className: ImageUploadWidget_scss_1.default.subtitle },
            intl.formatMessage(exports.messages.orClickToUpload)
          )
        )
      )
    );
  }
};
ImageUploadWidget = __decorate([mobx_react_1.observer], ImageUploadWidget);
exports.default = ImageUploadWidget;
//# sourceMappingURL=ImageUploadWidget.js.map
