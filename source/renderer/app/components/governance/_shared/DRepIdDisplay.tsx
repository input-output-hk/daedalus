import React, { useCallback, useMemo, useState } from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { Tooltip } from 'react-polymorph/lib/components/Tooltip';
import { TooltipSkin } from 'react-polymorph/lib/skins/simple/TooltipSkin';
import { logger } from '../../../utils/logging';
import { normalizeDRepIdentity } from '../../../utils/governance/normalizeDRepIdentity';
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
  cip105Caption: {
    id: 'governance.drepDirectory.cip105Caption',
    defaultMessage: '!!!(CIP-105)',
    description: 'Caption in front of the CIP-105 form of a DRep ID',
  },
  copyLabelCip129: {
    id: 'governance.drepDirectory.copyIdCip129',
    defaultMessage: '!!!Copy CIP-129 DRep ID',
    description: 'Accessible label for the CIP-129 copy button',
  },
  copyLabelCip105: {
    id: 'governance.drepDirectory.copyIdCip105',
    defaultMessage: '!!!Copy CIP-105 DRep ID',
    description: 'Accessible label for the CIP-105 copy button',
  },
  idAriaCip129: {
    id: 'governance.drepDirectory.idAriaCip129',
    defaultMessage: '!!!CIP-129 DRep ID {value}',
    description: 'Accessible label of the CIP-129 DRep ID value',
  },
  idAriaCip105: {
    id: 'governance.drepDirectory.idAriaCip105',
    defaultMessage: '!!!CIP-105 DRep ID {value}',
    description: 'Accessible label of the CIP-105 DRep ID value',
  },
});

export type DRepIdDisplayVariant = 'single' | 'stacked' | 'full';

interface Props {
  drepId: string;
  variant?: DRepIdDisplayVariant;
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
  variant = 'single',
  showCopiedConfirmation = false,
  intl,
}: Props) {
  const [copied, setCopied] = useState(false);

  const copyValue = useCallback((value: string) => {
    if (!navigator.clipboard || !navigator.clipboard.writeText) {
      logger.warn('DRepIdDisplay: clipboard API is unavailable', {
        drepIdLength: value.length,
      });
      return;
    }

    navigator.clipboard
      .writeText(value)
      .then(() => setCopied(true))
      .catch((error) => {
        logger.warn('DRepIdDisplay: failed to copy DRep ID', {
          error,
          drepIdLength: value.length,
        });
      });
  }, []);

  const handleCopy = useCallback(() => copyValue(drepId), [copyValue, drepId]);

  // The legacy form is derived per render, never stored or handed on: an id
  // the decoder rejects simply has no second row.
  const cip105 = useMemo(
    () =>
      variant === 'single'
        ? null
        : (normalizeDRepIdentity(drepId)?.cip105 ?? null),
    [drepId, variant]
  );

  const confirmation = showCopiedConfirmation && copied && (
    <span
      className={styles.copiedConfirmation}
      role="status"
      aria-live="polite"
    >
      {intl.formatMessage(messages.copiedToast)}
    </span>
  );

  if (variant === 'single') {
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
        {confirmation}
      </span>
    );
  }

  const isFull = variant === 'full';

  return (
    <span className={styles.stack}>
      <span className={styles.formRow}>
        {isFull ? (
          <code
            className={styles.idFull}
            aria-label={intl.formatMessage(messages.idAriaCip129, {
              value: drepId,
            })}
          >
            {drepId}
          </code>
        ) : (
          <Tooltip tip={drepId} skin={TooltipSkin} isAligningRight={false}>
            <code
              className={styles.id}
              aria-label={intl.formatMessage(messages.idAriaCip129, {
                value: drepId,
              })}
            >
              {truncateId(drepId)}
            </code>
          </Tooltip>
        )}
        <Button
          className={styles.copyButton}
          onClick={handleCopy}
          label={intl.formatMessage(messages.copyButton)}
          skin={ButtonSkin}
          aria-label={intl.formatMessage(messages.copyLabelCip129)}
        />
      </span>
      {cip105 !== null && (
        <span className={styles.formRow}>
          <span className={styles.formCaption}>
            {intl.formatMessage(messages.cip105Caption)}
          </span>
          {isFull ? (
            <code
              className={styles.idFull}
              aria-label={intl.formatMessage(messages.idAriaCip105, {
                value: cip105,
              })}
            >
              {cip105}
            </code>
          ) : (
            <Tooltip tip={cip105} skin={TooltipSkin} isAligningRight={false}>
              <code
                className={styles.id}
                aria-label={intl.formatMessage(messages.idAriaCip105, {
                  value: cip105,
                })}
              >
                {truncateId(cip105)}
              </code>
            </Tooltip>
          )}
          <Button
            className={styles.copyButton}
            onClick={() => copyValue(cip105)}
            label={intl.formatMessage(messages.copyButton)}
            skin={ButtonSkin}
            aria-label={intl.formatMessage(messages.copyLabelCip105)}
          />
        </span>
      )}
      {confirmation}
    </span>
  );
}

export default injectIntl(DRepIdDisplay);
