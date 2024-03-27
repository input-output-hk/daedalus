'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.buildKnownIssueFixesSubmenu = void 0;
const electron_1 = require('electron');
const rtsFlagsSettings_1 = require('../utils/rtsFlagsSettings');
const environment_1 = require('../environment');
const getTranslation_1 = require('../utils/getTranslation');
const buildKnownIssueFixesSubmenu = (
  actions,
  translations,
  translate = (0, getTranslation_1.getTranslation)(translations, 'menu')
) => {
  const { isBlankScreenFixActive, network } = environment_1.environment;
  const rtsFlags = (0, rtsFlagsSettings_1.getRtsFlagsSettings)(network);
  const areRTSFlagsEnabled = !!rtsFlags?.length && rtsFlags.length > 0;
  return [
    // @ts-ignore ts-migrate(2740) FIXME: Type '{ label: any; click(): void; }' is missing t... Remove this comment to see the full error message
    {
      label: translate('helpSupport.knownIssues'),
      click() {
        const faqLink = translate('helpSupport.knownIssuesUrl');
        electron_1.shell.openExternal(faqLink);
      },
    },
    // @ts-ignore ts-migrate(2740) FIXME: Type '{ label: any; type: "checkbox"; checked: boo... Remove this comment to see the full error message
    {
      label: translate('helpSupport.blankScreenFix'),
      type: 'checkbox',
      checked: isBlankScreenFixActive,
      click(item) {
        actions.toggleBlankScreenFix(item);
      },
    },
    // @ts-ignore ts-migrate(2740) FIXME: Type '{ label: any; type: "checkbox"; checked: boo... Remove this comment to see the full error message
    {
      label: translate('helpSupport.usingRtsFlags'),
      type: 'checkbox',
      checked: areRTSFlagsEnabled,
      click(item) {
        actions.openToggleRTSFlagsModeDialog(!areRTSFlagsEnabled);
        // keep previous setting until app restart
        item.checked = areRTSFlagsEnabled;
      },
    },
  ];
};
exports.buildKnownIssueFixesSubmenu = buildKnownIssueFixesSubmenu;
//# sourceMappingURL=submenuBuilders.js.map
