'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importDefault(require('react'));
const mobx_react_1 = require('mobx-react');
const discreet_mode_1 = require('../../../features/discreet-mode');
const SecuritySettings_1 = __importDefault(
  require('../../../components/settings/categories/SecuritySettings')
);
function SecuritySettingsPage() {
  const discreetModeFeature = (0, discreet_mode_1.useDiscreetModeFeature)();
  return react_1.default.createElement(SecuritySettings_1.default, {
    discreetMode: discreetModeFeature.isDiscreetMode,
    openDiscreetMode: discreetModeFeature.openInDiscreetMode,
    onDiscreetModeToggle: discreetModeFeature.toggleDiscreetMode,
    onOpenDiscreetModeToggle: discreetModeFeature.toggleOpenInDiscreetMode,
  });
}
exports.default = (0, mobx_react_1.observer)(SecuritySettingsPage);
//# sourceMappingURL=SecuritySettingsPage.js.map
