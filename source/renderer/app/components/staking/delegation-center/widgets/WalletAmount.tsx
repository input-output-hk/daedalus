import React from 'react';
import { FormattedHTMLMessage } from 'react-intl';
import { useDiscreetModeFeature } from '../../../../features/discreet-mode';
import type { ReactIntlMessage } from '../../../../types/i18nTypes';

type Props = {
  amount: string;
  walletAmount: ReactIntlMessage;
};
export function WalletAmount({ amount, walletAmount }: Props) {
  const discreetModeFeature = useDiscreetModeFeature();
  return (
    <FormattedHTMLMessage
      {...walletAmount}
      values={{
        amount: discreetModeFeature.discreetValue({
          value: amount,
        }),
      }}
    />
  );
}
