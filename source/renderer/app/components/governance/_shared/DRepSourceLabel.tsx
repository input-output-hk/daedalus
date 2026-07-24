import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';

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
});

export type DRepSourceLabelVariant = 'on-chain' | 'on-chain-anchor-reference';

type Props = {
  source: DRepSourceLabelVariant;
  className?: string;
  intl: intlShape.isRequired;
};

function DRepSourceLabel({ source, className, intl }: Props) {
  const messageBySource = {
    'on-chain': messages.onChain,
    'on-chain-anchor-reference': messages.anchorReference,
  };
  const message = messageBySource[source];
  if (!message) return null;

  return <span className={className}>{intl.formatMessage(message)}</span>;
}

export default injectIntl(DRepSourceLabel);
