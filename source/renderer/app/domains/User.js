'use strict';
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
const mobx_1 = require('mobx');
const Profile_1 = __importDefault(require('./Profile'));
class User {
  id;
  profile;
  constructor(id, profile) {
    this.id = id;
    this.profile = profile;
  }
}
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  User.prototype,
  'id',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Profile_1.default)],
  User.prototype,
  'profile',
  void 0
);
exports.default = User;
//# sourceMappingURL=User.js.map
