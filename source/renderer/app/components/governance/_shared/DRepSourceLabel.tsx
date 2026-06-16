import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';

const messages = defineMessages({
  onChain: {
    id: 'governance.drepDirectory.source.onChain',
    defaultMessage: '!!!On-chain',
    description: 'Source label for on-chain DRep data',
  },
});

type Props = {
  source: 'on-chain';
  className?: string;
  intl: intlShape.isRequired;
};

function DRepSourceLabel({ source, className, intl }: Props) {
  if (source !== 'on-chain') {
    return null;
  }

  return (
    <span className={className}>{intl.formatMessage(messages.onChain)}</span>
  );
}

export default injectIntl(DRepSourceLabel);
