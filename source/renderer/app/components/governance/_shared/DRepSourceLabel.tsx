import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import type { MessageDescriptor } from 'react-intl';

const messages = defineMessages({
  onChain: {
    id: 'governance.drepDirectory.source.onChain',
    defaultMessage: '!!!On-chain',
    description: 'Source label for on-chain DRep data',
  },
  anchorReference: {
    id: 'governance.drepDetail.sourceLabel.anchorReference',
    defaultMessage: '!!!On-chain anchor reference',
    description:
      'Source label for the raw anchor URL and hash pair recorded on-chain',
  },
  verified: {
    id: 'governance.drepDetail.sourceLabel.verified',
    defaultMessage: '!!!Verified off-chain content',
    description: 'Source label for hash-verified anchor content',
  },
  verifiedTooltip: {
    id: 'governance.drepDetail.sourceLabel.verified.tooltip',
    defaultMessage:
      '!!!Fetched from {host}, hash-matched the on-chain anchor hash.',
    description: 'Tooltip for the verified off-chain content source label',
  },
  unverified: {
    id: 'governance.drepDetail.sourceLabel.unverified',
    defaultMessage: '!!!Unverified anchor',
    description: 'Source label for fetched but not yet hash-verified content',
  },
  unverifiedTooltip: {
    id: 'governance.drepDetail.sourceLabel.unverified.tooltip',
    defaultMessage:
      '!!!Anchor content fetched but not yet hash-verified. Treat as untrusted.',
    description: 'Tooltip for the unverified anchor source label',
  },
  anchorUnavailable: {
    id: 'governance.drepDetail.sourceLabel.anchorUnavailable',
    defaultMessage: '!!!Anchor unavailable',
    description: 'Source label when the anchor fetch or hash check failed',
  },
  anchorUnavailableTooltip: {
    id: 'governance.drepDetail.sourceLabel.anchorUnavailable.tooltip',
    defaultMessage:
      '!!!The anchor URL could not be retrieved or did not match the on-chain hash. Off-chain profile is not shown.',
    description: 'Tooltip for the anchor unavailable source label',
  },
});

export type DRepSourceLabelVariant =
  | 'on-chain'
  | 'on-chain-anchor-reference'
  | 'verified-off-chain'
  | 'unverified-anchor'
  | 'anchor-unavailable';

type Props = {
  source: DRepSourceLabelVariant;
  host?: string;
  className?: string;
  intl: intlShape.isRequired;
};

function DRepSourceLabel({ source, host, className, intl }: Props) {
  const messageBySource: Record<DRepSourceLabelVariant, MessageDescriptor> = {
    'on-chain': messages.onChain,
    'on-chain-anchor-reference': messages.anchorReference,
    'verified-off-chain': messages.verified,
    'unverified-anchor': messages.unverified,
    'anchor-unavailable': messages.anchorUnavailable,
  };
  const tooltipBySource: Partial<
    Record<DRepSourceLabelVariant, MessageDescriptor>
  > = {
    'verified-off-chain': messages.verifiedTooltip,
    'unverified-anchor': messages.unverifiedTooltip,
    'anchor-unavailable': messages.anchorUnavailableTooltip,
  };
  const message = messageBySource[source];
  if (!message) return null;

  const label = intl.formatMessage(message);
  const tooltipMessage = tooltipBySource[source];
  if (!tooltipMessage) {
    return <span className={className}>{label}</span>;
  }

  const tooltip = intl.formatMessage(tooltipMessage, { host: host ?? '' });
  return (
    <span
      className={className}
      title={tooltip}
      aria-label={`${label}. ${tooltip}`}
    >
      {label}
    </span>
  );
}

export default injectIntl(DRepSourceLabel);
