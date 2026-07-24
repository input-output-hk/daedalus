import React, { useCallback, useState } from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { Tooltip } from 'react-polymorph/lib/components/Tooltip';
import { TooltipSkin } from 'react-polymorph/lib/skins/simple/TooltipSkin';
import { logger } from '../../../utils/logging';
import styles from './DRepIdDisplay.scss';

const messages = defineMessages({
  copyButton: {
    id: 'governance.drepDirectory.copyButton',
    defaultMessage: '!!!Copy',
    description: 'Copy button label for a DRep ID',
  },
  copyLabel: {
    id: 'governance.drepDirectory.copyId',
    defaultMessage: '!!!Copy DRep ID',
    description: 'Accessible label for the DRep ID copy button',
  },
  copiedToast: {
    id: 'governance.drepDetail.copyIdToast',
    defaultMessage: '!!!DRep ID copied',
    description: 'Inline confirmation shown after copying a DRep ID',
  },
});

interface Props {
  drepId: string;
  showCopiedConfirmation?: boolean;
  intl: intlShape.isRequired;
}

/** Maximum display length before truncation. */
const MAX_DISPLAY_LENGTH = 18;
const PREFIX_LENGTH = 8;
const SUFFIX_LENGTH = 6;

function truncateId(id: string): string {
  if (id.length <= MAX_DISPLAY_LENGTH) return id;
  return `${id.slice(0, PREFIX_LENGTH)}…${id.slice(-SUFFIX_LENGTH)}`;
}

function DRepIdDisplay({
  drepId,
  showCopiedConfirmation = false,
  intl,
}: Props) {
  const [copied, setCopied] = useState(false);
  const handleCopy = useCallback(() => {
    if (!navigator.clipboard || !navigator.clipboard.writeText) {
      logger.warn('DRepIdDisplay: clipboard API is unavailable', {
        drepIdLength: drepId.length,
      });
      return;
    }

    navigator.clipboard
      .writeText(drepId)
      .then(() => setCopied(true))
      .catch((error) => {
        logger.warn('DRepIdDisplay: failed to copy DRep ID', {
          error,
          drepIdLength: drepId.length,
        });
      });
  }, [drepId]);

  const truncated = truncateId(drepId);

  return (
    <span className={styles.container}>
      <Tooltip tip={drepId} skin={TooltipSkin} isAligningRight={false}>
        <code className={styles.id} aria-label={drepId}>
          {truncated}
        </code>
      </Tooltip>
      <Button
        className={styles.copyButton}
        onClick={handleCopy}
        label={intl.formatMessage(messages.copyButton)}
        skin={ButtonSkin}
        aria-label={intl.formatMessage(messages.copyLabel)}
      />
      {showCopiedConfirmation && copied && (
        <span
          className={styles.copiedConfirmation}
          role="status"
          aria-live="polite"
        >
          {intl.formatMessage(messages.copiedToast)}
        </span>
      )}
    </span>
  );
}

export default injectIntl(DRepIdDisplay);
