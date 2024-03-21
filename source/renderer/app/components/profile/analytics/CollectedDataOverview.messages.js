'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.messages = void 0;
const react_intl_1 = require('react-intl');
exports.messages = (0, react_intl_1.defineMessages)({
  title: {
    id: 'analytics.form.dataCollectionDetailsTitle',
    defaultMessage:
      '!!!We collect data on (1) User click behavior and (2) Device information.',
    description: 'Data collection details title',
  },
  userBehaviorTitle: {
    id: 'analytics.form.dataCollectionDetailsUserBehaviourTitle',
    defaultMessage: '!!!User click behavior',
    description: 'Title for the user behaviour data collection',
  },
  userBehaviorText: {
    id: 'analytics.form.dataCollectionDetailsUserBehaviorText',
    defaultMessage:
      '!!!Clicks, page visits, page scrolling, number of wallets, number of native assets, session duration, type of wallets (soft vs hardware wallets), geolocation (country of location), and page performance.',
    description: 'Description for the user behaviour data collection',
  },
  deviceInfoTitle: {
    id: 'analytics.form.dataCollectionDetailsDeviceInfoTitle',
    defaultMessage: '!!!Device info',
    description: 'Title for the device info data collection',
  },
  deviceInfoText: {
    id: 'analytics.form.dataCollectionDetailsDeviceInfoText',
    defaultMessage: '!!!Operating system, RAM, and disk space.',
    description: 'Description for the device info data collection',
  },
  expandButton: {
    id: 'analytics.dialog.expandButton',
    defaultMessage: '!!!Expand details',
    description: 'Expand details button',
  },
  collapseButton: {
    id: 'analytics.dialog.collapseButton',
    defaultMessage: '!!!Collapse details',
    description: 'Collapse details button',
  },
});
//# sourceMappingURL=CollectedDataOverview.messages.js.map
