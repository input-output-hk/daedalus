import React, { useCallback, useState } from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import DRepSourceLabel from '../_shared/DRepSourceLabel';
import { isHttpsUrl } from '../../../utils/governance/isHttpsUrl';
import type { AppDRepDetail } from '../../../stores/GovernanceStore';
import styles from './DRepDetail.scss';

type DRepMetadata = NonNullable<AppDRepDetail['metadata']>;
type DRepReference = DRepMetadata['references'][number];

const messages = defineMessages({
  title: {
    id: 'governance.drepDetail.anchorContent.title',
    defaultMessage: '!!!Off-chain profile',
    description: 'Heading of the verified anchor content block',
  },
  givenName: {
    id: 'governance.drepDetail.anchorContent.givenName',
    defaultMessage: '!!!Name',
    description: 'Label for the verified CIP-119 givenName field',
  },
  objectives: {
    id: 'governance.drepDetail.anchorContent.objectives',
    defaultMessage: '!!!Objectives',
    description: 'Label for the verified CIP-119 objectives field',
  },
  motivations: {
    id: 'governance.drepDetail.anchorContent.motivations',
    defaultMessage: '!!!Motivations',
    description: 'Label for the verified CIP-119 motivations field',
  },
  qualifications: {
    id: 'governance.drepDetail.anchorContent.qualifications',
    defaultMessage: '!!!Qualifications',
    description: 'Label for the verified CIP-119 qualifications field',
  },
  referencesTitle: {
    id: 'governance.drepDetail.anchorContent.references.title',
    defaultMessage: '!!!References',
    description: 'Heading of the verified CIP-119 references block',
  },
  referencesLinks: {
    id: 'governance.drepDetail.anchorContent.references.links',
    defaultMessage: '!!!Links',
    description: 'Sub-heading for references typed as Link',
  },
  referencesIdentity: {
    id: 'governance.drepDetail.anchorContent.references.identity',
    defaultMessage: '!!!Claimed identities',
    description: 'Sub-heading for references typed as Identity',
  },
  referencesIdentityCaption: {
    id: 'governance.drepDetail.anchorContent.references.identityCaption',
    defaultMessage:
      '!!!These identities are claimed by the DRep and are not verified by Daedalus. Open the link and confirm that this DRep ID is published there before you rely on it.',
    description:
      'Caption stating that an Identity reference is a claim, not a verified identity',
  },
paymentAddressLabel: {
    id: 'governance.drepDetail.anchorContent.paymentAddress.label',
    defaultMessage: '!!!Stated payment address',
    description: 'Label for the verified CIP-119 paymentAddress field',
  },
  paymentAddressCaption: {
    id: 'governance.drepDetail.anchorContent.paymentAddress.caption',
    defaultMessage:
      "!!!This address is the DRep's own claim. Delegating your voting power requires no payment to any address.",
    description:
      'Caption warning that no payment is required to delegate voting power',
  },
  paymentAddressCopyButton: {
    id: 'governance.drepDetail.anchorContent.paymentAddress.copyButton',
    defaultMessage: '!!!Copy',
    description: 'Copy button label for the stated payment address',
  },
  paymentAddressCopyLabel: {
    id: 'governance.drepDetail.anchorContent.paymentAddress.copyLabel',
    defaultMessage: '!!!Copy stated payment address',
    description: 'Accessible label for the payment address copy button',
  },
  paymentAddressCopiedToast: {
    id: 'governance.drepDetail.anchorContent.paymentAddress.copiedToast',
    defaultMessage: '!!!Payment address copied',
    description: 'Inline confirmation shown after copying the payment address',
  },
  caption: {
    id: 'governance.drepDetail.anchorContent.caption',
    defaultMessage:
      "!!!This name is the DRep's own claim, hash-matched to the anchor recorded on-chain. Daedalus does not verify identity.",
    description:
      'Caption stating that a verified name is not verified identity',
  },
});

interface Props {
  verifiedName: string | null;
  metadata: AppDRepDetail['metadata'];
  anchorUrl: string | null;
  onOpenExternalLink: (url: string) => void;
  intl: intlShape.isRequired;
}

function deriveHost(anchorUrl: string | null): string {
  if (!anchorUrl) return '';
  try {
    return new URL(anchorUrl).hostname;
  } catch {
    return '';
  }
}

function VerifiedFieldRow({
  host,
  label,
  value,
}: {
  host: string;
  label: string;
  value: string;
}) {
  return (
    <div className={styles.fieldRow}>
      <dt className={styles.fieldLabel}>{label}</dt>
      <dd className={styles.fieldValue}>
        {value}{' '}
        <DRepSourceLabel
          source="verified-off-chain"
          host={host}
          className={styles.sourceLabel}
        />
      </dd>
    </div>
  );
}

function ReferenceList({
  onOpenExternalLink,
  references,
}: {
  onOpenExternalLink: (url: string) => void;
  references: DRepReference[];
}) {
  return (
    <ul className={styles.referenceList}>
      {references.map((reference, index) => (
        <li
          className={styles.referenceItem}
          key={`${reference.uri}-${index}`} // eslint-disable-line react/no-array-index-key
        >
          {isHttpsUrl(reference.uri) ? (
            <a
              href={reference.uri}
              target="_blank"
              rel="noopener noreferrer"
              onClick={(event: React.MouseEvent<HTMLAnchorElement>) => {
                event.preventDefault();
                onOpenExternalLink(reference.uri);
              }}
            >
              {reference.label ?? reference.uri}
            </a>
          ) : (
            <span className={styles.anchorValue}>
              {reference.label ?? reference.uri}
            </span>
          )}
        </li>
      ))}
    </ul>
  );
}

function DRepDetailAnchorContent({
  verifiedName,
  metadata,
  anchorUrl,
  onOpenExternalLink,
  intl,
}: Props) {
  const [addressCopied, setAddressCopied] = useState(false);

  // Nothing on this path is logged: a payment address is a bech32 string and the
  // sanitization floor forbids it in any logger payload, including a length.
  const handleCopyPaymentAddress = useCallback(() => {
    const addr = metadata?.paymentAddress;
    if (addr == null) return;
    if (!navigator.clipboard || !navigator.clipboard.writeText) return;
    navigator.clipboard
      .writeText(addr)
      .then(() => setAddressCopied(true))
      .catch(() => undefined);
  }, [metadata?.paymentAddress]);

  if (!metadata && verifiedName == null) return null;

  const host = deriveHost(anchorUrl);
  const references = metadata?.references ?? [];
  const linkReferences = references.filter((r) => r.type === 'link' || r.type === 'other');
  const identityReferences = references.filter((r) => r.type === 'identity');
  const hasFieldRows =
    verifiedName != null ||
    metadata?.objectives != null ||
    metadata?.motivations != null ||
    metadata?.qualifications != null;
  const hasAnyContent =
    hasFieldRows || references.length > 0 || metadata?.paymentAddress != null;

  if (!hasAnyContent) return null;

  return (
    <>
      <h3 className={styles.sectionTitle}>
        {intl.formatMessage(messages.title)}
      </h3>
      {hasFieldRows && (
        <dl className={styles.fieldList}>
          {verifiedName != null && (
            <VerifiedFieldRow
              host={host}
              label={intl.formatMessage(messages.givenName)}
              value={verifiedName}
            />
          )}
          {metadata?.objectives != null && (
            <VerifiedFieldRow
              host={host}
              label={intl.formatMessage(messages.objectives)}
              value={metadata.objectives}
            />
          )}
          {metadata?.motivations != null && (
            <VerifiedFieldRow
              host={host}
              label={intl.formatMessage(messages.motivations)}
              value={metadata.motivations}
            />
          )}
          {metadata?.qualifications != null && (
            <VerifiedFieldRow
              host={host}
              label={intl.formatMessage(messages.qualifications)}
              value={metadata.qualifications}
            />
          )}
        </dl>
      )}
      {verifiedName != null && (
        <p className={styles.mutedValue}>
          {intl.formatMessage(messages.caption)}
        </p>
      )}
      {references.length > 0 && (
        <>
          <h4 className={styles.subSectionTitle}>
            {intl.formatMessage(messages.referencesTitle)}
          </h4>
          {linkReferences.length > 0 && (
            <>
              <h5 className={styles.subSectionTitle}>
                {intl.formatMessage(messages.referencesLinks)}{' '}
                <DRepSourceLabel
                  source="verified-off-chain"
                  host={host}
                  className={styles.sourceLabel}
                />
              </h5>
              <ReferenceList
                onOpenExternalLink={onOpenExternalLink}
                references={linkReferences}
              />
            </>
          )}
          {identityReferences.length > 0 && (
            <>
              <h5 className={styles.subSectionTitle}>
                {intl.formatMessage(messages.referencesIdentity)}{' '}
                <DRepSourceLabel
                  source="verified-off-chain"
                  host={host}
                  className={styles.sourceLabel}
                />
              </h5>
              <p className={styles.mutedValue}>
                {intl.formatMessage(messages.referencesIdentityCaption)}
              </p>
              <ReferenceList
                onOpenExternalLink={onOpenExternalLink}
                references={identityReferences}
              />
            </>
          )}
        </>
      )}
      {metadata?.paymentAddress != null && (
        <>
          <h4 className={styles.subSectionTitle}>
            {intl.formatMessage(messages.paymentAddressLabel)}{' '}
            <DRepSourceLabel
              source="verified-off-chain"
              host={host}
              className={styles.sourceLabel}
            />
          </h4>
          <p className={styles.mutedValue}>
            {intl.formatMessage(messages.paymentAddressCaption)}
          </p>
          <p className={styles.paymentAddressValue}>
            <span>{metadata.paymentAddress}</span>
            <Button
              className={styles.copyButton}
              onClick={handleCopyPaymentAddress}
              label={intl.formatMessage(messages.paymentAddressCopyButton)}
              skin={ButtonSkin}
              aria-label={intl.formatMessage(messages.paymentAddressCopyLabel)}
            />
            {addressCopied && (
              <span
                className={styles.copiedConfirmation}
                role="status"
                aria-live="polite"
              >
                {intl.formatMessage(messages.paymentAddressCopiedToast)}
              </span>
            )}
          </p>
        </>
      )}
    </>
  );
}

export default injectIntl(DRepDetailAnchorContent);
