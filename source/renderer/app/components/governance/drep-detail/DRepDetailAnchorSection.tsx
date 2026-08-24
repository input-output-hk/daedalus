import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import DRepSectionHeading from '../_shared/DRepSectionHeading';
import DRepExternalLink from '../_shared/DRepExternalLink';
import DRepDetailAnchorContent from './DRepDetailAnchorContent';
import { isHttpsUrl } from '../../../utils/governance/isHttpsUrl';
import type { DRepAnchorPresence } from '../../../../../common/types/governance.types';
import type { AppDRepDetail } from '../../../stores/GovernanceStore';
import styles from './DRepDetail.scss';

const messages = defineMessages({
  emptyBadge: {
    id: 'governance.drepDetail.anchor.empty.badge',
    defaultMessage: '!!!Nothing published',
    description:
      'Badge on a DRep whose anchor verified but carried no readable fields',
  },
  emptyExplanation: {
    id: 'governance.drepDetail.anchor.empty.explanation',
    defaultMessage:
      "!!!Daedalus retrieved this document and it matched the anchor hash on chain, so it is genuinely this DRep's. It carries none of the fields a profile is built from, so there is nothing here to read.",
    description:
      'Explains that a verified anchor document contained no usable content',
  },
  unverifiedBadge: {
    id: 'governance.drepDetail.anchor.unverified.badge',
    defaultMessage: '!!!Unverified',
    description:
      'Badge on a DRep that registered an anchor whose contents did not verify',
  },
  unverifiedExplanation: {
    id: 'governance.drepDetail.anchor.unverified.explanation',
    defaultMessage:
      "!!!This DRep registered an anchor URL and a content hash on chain. Daedalus could not retrieve a document matching that hash, so nothing from it is shown here. The link is left in place so you can look for yourself, but whatever is published there is this DRep's claim alone: Daedalus has not checked it and cannot confirm it.",
    description:
      'Explains why the off-chain contents of an unverified anchor are withheld',
  },
  titleExplanation: {
    id: 'governance.drepDetail.anchor.title.explanation',
    defaultMessage:
      "!!!Published by the DRep at the anchor recorded on-chain, and shown only after it matches the on-chain hash. Everything here is the DRep's own claim: Daedalus verifies that the content is what was published, not that it is true.",
    description: 'Explains what the off-chain metadata section contains',
  },
  title: {
    id: 'governance.drepDetail.anchor.title',
    defaultMessage: '!!!Anchor',
    description: 'Heading of the anchor section on the DRep detail view',
  },
  urlLabel: {
    id: 'governance.drepDetail.anchor.url',
    defaultMessage: '!!!Anchor URL',
    description: 'Label for the on-chain anchor URL field',
  },
  hashLabel: {
    id: 'governance.drepDetail.anchor.hash',
    defaultMessage: '!!!Anchor hash',
    description: 'Label for the on-chain anchor hash field',
  },
  sourceRowLabel: {
    id: 'governance.drepDetail.anchor.source',
    defaultMessage: '!!!Source',
    description: 'Label for the anchor source-label row',
  },
  none: {
    id: 'governance.drepDetail.anchor.none',
    defaultMessage: '!!!No anchor is recorded on-chain for this DRep.',
    description: 'Shown when the DRep registered without an anchor',
  },
});

interface Props {
  anchor: DRepAnchorPresence | null;
  verifiedName: string | null;
  metadata: AppDRepDetail['metadata'];
  network?: string | null;
  onOpenExternalLink: (url: string) => void;
  intl: intlShape.isRequired;
}

function DRepDetailAnchorSection({
  anchor,
  verifiedName,
  metadata,
  network,
  onOpenExternalLink,
  intl,
}: Props) {
  // cardano-wallet fetches the anchor and checks it against the on-chain hash
  // itself, returning no metadata when that fails. An anchor with nothing
  // behind it therefore means unverified rather than empty.
  // Three outcomes, not two. An anchor that did not verify is a different fact
  // from one that verified and turned out to hold nothing, and both are
  // different from a profile. Collapsing the middle case into "no content" left
  // the section showing a URL, a hash, and then silence.
  const isUnverified =
    anchor != null && metadata == null && verifiedName == null;
  const isEmpty =
    anchor != null &&
    !isUnverified &&
    verifiedName == null &&
    metadata != null &&
    metadata.objectives == null &&
    metadata.motivations == null &&
    metadata.qualifications == null &&
    metadata.paymentAddress == null &&
    (metadata.references?.length ?? 0) === 0;

  return (
    <section
      className={styles.section}
      aria-label={intl.formatMessage(messages.title)}
    >
      <DRepSectionHeading
        title={intl.formatMessage(messages.title)}
        explanation={intl.formatMessage(messages.titleExplanation)}
      />
      {anchor ? (
        <>
          <dl className={styles.fieldList}>
            <div className={styles.fieldRow}>
              <dt className={styles.fieldLabel}>
                {intl.formatMessage(messages.urlLabel)}
              </dt>
              <dd className={styles.anchorValue}>
                {isHttpsUrl(anchor.url) ? (
                  <DRepExternalLink
                    url={anchor.url}
                    onOpenExternalLink={onOpenExternalLink}
                  />
                ) : (
                  anchor.url
                )}
              </dd>
            </div>
            <div className={styles.fieldRow}>
              <dt className={styles.fieldLabel}>
                {intl.formatMessage(messages.hashLabel)}
              </dt>
              <dd className={styles.anchorValue}>{anchor.hash}</dd>
            </div>
          </dl>
          {/* An anchor whose contents did not verify leaves this section
              showing a URL and a hash and nothing else, which reads as though
              the DRep simply wrote nothing. Saying so is the one place a badge
              earns its keep here: it marks the exception, and what is being
              withheld is exactly what a reader would otherwise trust. */}
          {isUnverified || isEmpty ? (
            <div className={styles.unverifiedNotice}>
              <span
                className={
                  isUnverified ? styles.unverifiedBadge : styles.emptyBadge
                }
              >
                {isUnverified && <span aria-hidden="true">⚠</span>}
                {intl.formatMessage(
                  isUnverified ? messages.unverifiedBadge : messages.emptyBadge
                )}
              </span>
              <p className={styles.unverifiedExplanation}>
                {intl.formatMessage(
                  isUnverified
                    ? messages.unverifiedExplanation
                    : messages.emptyExplanation
                )}
              </p>
            </div>
          ) : (
            <DRepDetailAnchorContent
              verifiedName={verifiedName}
              metadata={metadata}
              network={network}
              onOpenExternalLink={onOpenExternalLink}
            />
          )}
        </>
      ) : (
        <p className={styles.mutedValue}>{intl.formatMessage(messages.none)}</p>
      )}
    </section>
  );
}

export default injectIntl(DRepDetailAnchorSection);
