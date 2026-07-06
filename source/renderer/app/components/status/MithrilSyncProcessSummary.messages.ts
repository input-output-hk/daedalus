import { defineMessages } from 'react-intl';
import type { ReactIntlMessage } from '../../types/i18nTypes';

/**
 * Canonical shutdown/restore/restart summary sentence, kept behind one i18n key
 * and shared by the partial-sync confirmation modal and the prompt confirm view;
 * import this module rather than redeclaring the id.
 */
const messages: Record<string, ReactIntlMessage> = defineMessages({
  processSummary: {
    id: 'daedalus.diagnostics.dialog.mithrilSyncProcessSummary',
    defaultMessage:
      '!!!For this process to begin your Cardano node will need to be shut down. Mithril will then be used to sync the verified chain data. On Mithril Sync completion, the node will be restarted to sync the remaining blocks.',
    description:
      'Canonical shutdown/restore/restart summary sentence shared by the Mithril partial sync confirmation modal and the proactive prompt confirmation view.',
  },
});

export default messages;
