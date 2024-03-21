'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const Action_1 = __importDefault(require('./lib/Action'));
class AssetsActions {
  setEditedAsset = new Action_1.default();
  onAssetSettingsSubmit = new Action_1.default();
  unsetEditedAsset = new Action_1.default();
  onOpenAssetSend = new Action_1.default();
  onCopyAssetParam = new Action_1.default();
  onToggleFavorite = new Action_1.default();
  copyAssetParamNotification = new Action_1.default();
}
exports.default = AssetsActions;
//# sourceMappingURL=assets-actions.js.map
