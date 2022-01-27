// @flow
import { shell } from 'electron';
import type { MenuItem } from 'electron';
import { getRtsFlagsSettings } from '../utils/rtsFlagsSettings';
import { environment } from '../environment';
import type { MenuActions } from './MenuActions.types';
import { getTranslation } from '../utils/getTranslation';

export const buildKnownIssueFixesSubmenu = (
  actions: MenuActions,
  translations: {},
  translate: Function = getTranslation(translations, 'menu')
): MenuItem[] => {
  const { isBlankScreenFixActive, network } = environment;
  const rtsFlags = getRtsFlagsSettings(network);
  const rtsFlagsEnabled = !!rtsFlags?.length && rtsFlags.length > 0;

  return [
    {
      label: translate('helpSupport.knownIssues'),
      click() {
        const faqLink = translate('helpSupport.knownIssuesUrl');
        shell.openExternal(faqLink);
      },
    },
    {
      label: translate('helpSupport.blankScreenFix'),
      type: 'checkbox',
      checked: isBlankScreenFixActive,
      click(item) {
        actions.toggleBlankScreenFix(item);
      },
    },
    {
      label: translate('helpSupport.usingRtsFlags'),
      type: 'checkbox',
      checked: rtsFlagsEnabled,
      click(item) {
        actions.openToggleRTSFlagsModeDialog(!rtsFlagsEnabled);
        // keep previous setting until app restart
        item.checked = rtsFlagsEnabled;
      },
    },
  ];
};
