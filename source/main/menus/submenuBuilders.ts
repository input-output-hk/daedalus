import { shell } from 'electron';
import type { MenuItem } from 'electron';
import { environment } from '../environment';
import type { MenuActions } from './MenuActions.types';
import { getTranslation } from '../utils/getTranslation';

export const buildKnownIssueFixesSubmenu = (
  actions: MenuActions,
  translations: {},
  rtsEnabled: boolean,
  translate: (...args: Array<any>) => any = getTranslation(translations, 'menu')
): MenuItem[] => {
  const { isBlankScreenFixActive } = environment;
  return [
    // @ts-ignore ts-migrate(2740) FIXME: Type '{ label: any; click(): void; }' is missing t... Remove this comment to see the full error message
    {
      label: translate('helpSupport.knownIssues'),

      click() {
        const faqLink = translate('helpSupport.knownIssuesUrl');
        shell.openExternal(faqLink);
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
      checked: rtsEnabled,

      click() {
        actions.openToggleRTSFlagsModeDialog(!rtsEnabled);
      },
    },
  ];
};
