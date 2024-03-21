'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const mobx_react_1 = require('mobx-react');
const rebuild_application_menu_1 = require('../../ipc/rebuild-application-menu');
const useMenuUpdater_1 = __importDefault(require('./useMenuUpdater'));
function MenuUpdaterComponent({
  stores: { app, profile, router, staking, uiDialogs },
}) {
  (0, useMenuUpdater_1.default)({
    stores: {
      app,
      profile,
      router,
      staking,
      uiDialogs,
    },
    rebuildApplicationMenu: rebuild_application_menu_1.rebuildApplicationMenu,
  });
  return null;
}
exports.default = (0, mobx_react_1.observer)(MenuUpdaterComponent);
//# sourceMappingURL=MenuUpdater.js.map
