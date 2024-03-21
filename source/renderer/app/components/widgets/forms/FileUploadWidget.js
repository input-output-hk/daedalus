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
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const path_1 = __importDefault(require('path'));
const show_file_dialog_channels_1 = require('../../../ipc/show-file-dialog-channels');
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/attach-... Remove this comment to see the full error message
const attach_ic_inline_svg_1 = __importDefault(
  require('../../../assets/images/attach-ic.inline.svg')
);
const FileUploadWidget_scss_1 = __importDefault(
  require('./FileUploadWidget.scss')
);
let FileUploadWidget = class FileUploadWidget extends react_1.Component {
  onOpen = async () => {
    const params = {
      filters: [
        {
          name: 'file-upload',
          extensions: this.props.acceptedFileTypes,
        },
      ],
      properties: ['openFile'],
    };
    const {
      filePaths,
    } = await show_file_dialog_channels_1.showOpenDialogChannel.send(params);
    if (!filePaths || filePaths.length === 0) {
      return;
    }
    const filePath = filePaths[0];
    this.props.onFileSelected(filePath);
  };
  onDragOver = () => false;
  onDragLeave = () => false;
  onDragEnd = () => false;
  onDrop = (e) => {
    e.preventDefault();
    const { files } = e.dataTransfer;
    if (!files || files.length === 0) {
      return;
    }
    const filePath = files[0].path;
    this.props.onFileSelected(filePath);
  };
  render() {
    const { label, placeholder, selectedFile } = this.props;
    const fileName = path_1.default.basename(selectedFile);
    return react_1.default.createElement(
      'div',
      { className: FileUploadWidget_scss_1.default.component },
      react_1.default.createElement(
        'div',
        { className: FileUploadWidget_scss_1.default.label },
        label
      ),
      react_1.default.createElement(
        'button',
        {
          className: FileUploadWidget_scss_1.default.dropZone,
          onClick: this.onOpen,
          onDragOver: this.onDragOver,
          onDragLeave: this.onDragLeave,
          onDragEnd: this.onDragEnd,
          onDrop: this.onDrop,
        },
        selectedFile
          ? react_1.default.createElement(
              'div',
              { className: FileUploadWidget_scss_1.default.fileName },
              fileName
            )
          : react_1.default.createElement(
              'div',
              { className: FileUploadWidget_scss_1.default.placeholder },
              placeholder
            ),
        react_1.default.createElement(react_svg_inline_1.default, {
          svg: attach_ic_inline_svg_1.default,
          className: FileUploadWidget_scss_1.default.attachIcon,
        })
      )
    );
  }
};
FileUploadWidget = __decorate([mobx_react_1.observer], FileUploadWidget);
exports.default = FileUploadWidget;
//# sourceMappingURL=FileUploadWidget.js.map
