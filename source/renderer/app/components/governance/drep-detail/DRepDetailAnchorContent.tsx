import React, { useCallback, useState } from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { isHttpsUrl } from '../../../utils/governance/isHttpsUrl';
import { isAddressForNetwork } from '../../../utils/governance/addressNetwork';
import DRepExternalLink from '../_shared/DRepExternalLink';
import type { AppDRepDetail } from '../../../stores/GovernanceStore';
import type {
  DRepAdditionalField,
  DRepAdditionalValue,
} from '../../../../../common/types/governance.types';
import DRepCopyButton from '../_shared/DRepCopyButton';
import DRepInfoIcon from '../_shared/DRepInfoIcon';
import styles from './DRepDetail.scss';

type DRepMetadata = NonNullable<AppDRepDetail['metadata']>;
type DRepReference = DRepMetadata['references'][number];

const messages = defineMessages({
  additionalTitle: {
    id: 'governance.drepDetail.anchorContent.additional.title',
    defaultMessage: '!!!Additional metadata fields',
    description: 'Heading of the non-canonical anchor content block',
  },
  additionalCaption: {
    id: 'governance.drepDetail.anchorContent.additional.caption',
    defaultMessage:
      '!!!These fields are not defined by any standard Daedalus recognises, so they appear under the names this DRep gave them and are shown exactly as published. Daedalus has not interpreted them.',
    description: 'Explains what the non-canonical metadata block contains',
  },
  paymentAddressWrongNetwork: {
    id: 'governance.drepDetail.anchorContent.paymentAddress.wrongNetwork',
    defaultMessage:
      '!!!This is not a {network} address. Nothing sent from this wallet can reach it.',
    description:
      'Warning shown when a DRep published a payment address for another network',
  },
  title: {
    id: 'governance.drepDetail.anchorContent.title',
    defaultMessage: '!!!Canonical metadata fields',
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
  network?: string | null;
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

/**
 * One CIP-119 prose field, drawn as a peer of References and the payment
 * address rather than as a labelled row.
 *
 * They are all fields of the same document, so reading them at different
 * weights implied a hierarchy the document does not have. The label-beside-
 * value shape was also the wrong one for the content: these run to 3,374
 * characters at their longest on mainnet, which a 120px label column squeezes
 * into a narrow ribbon down the page.
 */
/**
 * A field the DRep invented, and whatever structure they gave it.
 *
 * Rendered as a nested list rather than flattened or dropped. A multi-sig DRep
 * that publishes its members, each with a name and a title, has written
 * something a reader wants, and turning it into one string would lose which
 * name went with which title. Every leaf is text, and nothing in this block
 * becomes clickable, so the structure costs nothing the strings do not.
 */
function AdditionalFieldValue({
  value,
  depth,
}: {
  value: DRepAdditionalValue;
  depth: number;
}) {
  if (value.kind === 'text') {
    return <p className={styles.additionalValue}>{value.text}</p>;
  }
  if (value.kind === 'group') {
    return <AdditionalFieldList fields={value.fields} depth={depth + 1} />;
  }
  return (
    <ul className={styles.additionalList}>
      {value.items.map((item, index) => (
        // No stable identity exists inside a document nobody has defined.
        // eslint-disable-next-line react/no-array-index-key
        <li key={index} className={styles.additionalListItem}>
          <AdditionalFieldValue value={item} depth={depth + 1} />
        </li>
      ))}
    </ul>
  );
}

function AdditionalFieldList({
  fields,
  depth,
}: {
  fields: DRepAdditionalField[];
  depth: number;
}) {
  return (
    <>
      {fields.map((field) => (
        <div className={styles.additionalField} key={`${depth}-${field.key}`}>
          {/* The author's key, verbatim. Nobody can translate a term nobody
              has defined, so it is not dressed up as one of our labels. */}
          <h4 className={styles.additionalKey}>{field.key}</h4>
          <AdditionalFieldValue value={field.value} depth={depth} />
        </div>
      ))}
    </>
  );
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
    <div className={styles.proseField}>
      <h4 className={styles.subSectionTitle}>{label}</h4>
      <p className={styles.proseValue}>{value}</p>
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
            <DRepExternalLink
              url={reference.uri}
              label={reference.label}
              onOpenExternalLink={onOpenExternalLink}
            />
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
  network,
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

  // Null when there is nothing to compare, which is not a mismatch.
  const addressNetworkMatches =
    metadata?.paymentAddress != null
      ? isAddressForNetwork(metadata.paymentAddress, network)
      : null;

  if (!metadata && verifiedName == null) return null;

  const host = deriveHost(anchorUrl);
  const references = metadata?.references ?? [];
  const additionalFields = metadata?.additionalFields ?? [];
  const linkReferences = references.filter(
    (r) => r.type === 'link' || r.type === 'other'
  );
  const identityReferences = references.filter((r) => r.type === 'identity');
  // The name is the page heading now, not a row here, so counting it opened
  // this block over nothing for a DRep whose document carried a name and no
  // other field.
  const hasFieldRows =
    metadata?.objectives != null ||
    metadata?.motivations != null ||
    metadata?.qualifications != null;
  const hasAnyContent =
    hasFieldRows ||
    references.length > 0 ||
    additionalFields.length > 0 ||
    metadata?.paymentAddress != null;

  if (!hasAnyContent) return null;

  return (
    <>
      {/* Named for being defined somewhere rather than for CIP-119 by name,
          since a later CIP may define more and the heading should not have to
          be renamed when it does. The distinction from the other block is not
          pedantry: these labels are ours and are translated, while a field
          nobody has standardised can only be shown under the key its author
          chose. */}
      <h3 className={styles.blockTitle}>
        {intl.formatMessage(messages.title)}
      </h3>
      {hasFieldRows && (
        <div className={styles.fieldList}>
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
        </div>
      )}
      {/* Fields that hold one value come before the one that holds a
          list. A reader scanning down meets a bounded set first and an
          open-ended one after it, rather than having to scroll past as
          many references as a DRep chose to publish, eight at the most
          on mainnet, to reach a field that was always going to be
          there. */}
      {metadata?.paymentAddress != null && (
        <>
          <h4 className={styles.subSectionTitle}>
            {intl.formatMessage(messages.paymentAddressLabel)}{' '}
            <DRepInfoIcon
              explanation={intl.formatMessage(messages.paymentAddressCaption)}
            />
          </h4>
          {addressNetworkMatches === false && (
            // Cheap and worth it: the bech32 prefix carries the network, so a
            // string comparison catches an address nothing on this network can
            // pay. Silence here would leave someone to copy it and find out by
            // sending.
            <p className={styles.wrongNetworkWarning} role="status">
              <span aria-hidden="true">⚠</span>{' '}
              {intl.formatMessage(messages.paymentAddressWrongNetwork, {
                network: network ?? '',
              })}
            </p>
          )}
          <p className={styles.paymentAddressValue}>
            <span>{metadata.paymentAddress}</span>
            <DRepCopyButton
              onClick={handleCopyPaymentAddress}
              label={intl.formatMessage(messages.paymentAddressCopyLabel)}
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
      {(linkReferences.length > 0 || identityReferences.length > 0) && (
        <>
          <h4 className={styles.subSectionTitle}>
            {intl.formatMessage(messages.referencesTitle)}
          </h4>
          {linkReferences.length > 0 && (
            <>
              <h5 className={styles.subSubSectionTitle}>
                {intl.formatMessage(messages.referencesLinks)}{' '}
              </h5>
              <ReferenceList
                onOpenExternalLink={onOpenExternalLink}
                references={linkReferences}
              />
            </>
          )}
          {identityReferences.length > 0 && (
            <>
              <h5 className={styles.subSubSectionTitle}>
                {intl.formatMessage(messages.referencesIdentity)}{' '}
                <DRepInfoIcon
                  explanation={intl.formatMessage(
                    messages.referencesIdentityCaption
                  )}
                />
              </h5>
              <ReferenceList
                onOpenExternalLink={onOpenExternalLink}
                references={identityReferences}
              />
            </>
          )}
        </>
      )}
      {/* After the canonical block has closed, not inside it. Placed between
          the payment address and the references, this heading captured the
          references beneath it and made a field CIP-119 defines read as one
          the DRep invented. Each block sorts its own contents the same way,
          single values before collections. */}
      {additionalFields.length > 0 && (
        // Deliberately quieter than the block above it.
        // Our own chrome is what lends a field credibility: a key reading
        // "verifiedBy" with a value of "Cardano Foundation", drawn like
        // Objectives, would launder the DRep's own assertion into something
        // that looks checked. The digest proves the document is theirs, not
        // that anything in it is true.
        <section className={styles.additionalBlock}>
          <h3 className={styles.blockTitle}>
            {intl.formatMessage(messages.additionalTitle)}{' '}
            <DRepInfoIcon
              explanation={intl.formatMessage(messages.additionalCaption)}
            />
          </h3>
          <AdditionalFieldList fields={additionalFields} depth={0} />
        </section>
      )}
    </>
  );
}

export default injectIntl(DRepDetailAnchorContent);
