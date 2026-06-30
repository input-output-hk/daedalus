import { defineMessages } from 'react-intl';
import type { ReactIntlMessage } from '../../types/i18nTypes';

/**
 * Canonical D-702a-6 shutdown/restore/restart summary sentence.
 *
 * Shared by BOTH the Mithril partial sync confirmation modal
 * (`MithrilPartialSyncConfirmation.tsx`) and the proactive prompt confirmation
 * view (`SyncingConnectingMithrilPrompt.tsx`) so the byte-identical sentence
 * lives behind a single i18n key (`daedalus.diagnostics.dialog.mithrilSyncProcessSummary`).
 * Do NOT redeclare this id in either consumer — import this module instead.
 */
const messages: Record<string, ReactIntlMessage> = defineMessages({
  processSummary: {
    id: 'daedalus.diagnostics.dialog.mithrilSyncProcessSummary',
    defaultMessage:
      '!!!For this process to begin your Cardano node will need to be shutdown. Mithril will then be used to sync the verified chain data. On Mithril Sync completion, the node will be restarted to sync the remaining blocks.',
    description:
      'Canonical shutdown/restore/restart summary sentence shared by the Mithril partial sync confirmation modal and the proactive prompt confirmation view.',
  },
});

export default messages;
