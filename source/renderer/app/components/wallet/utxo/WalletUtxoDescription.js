// @flow
import React from 'react';
import { FormattedHTMLMessage } from 'react-intl';
import { useDiscreetModeFeature } from '../../../features/discreet-mode';
import type { ReactIntlMessage } from '../../../types/i18nTypes';

type Props = {
  description: ReactIntlMessage,
  formattedWalletAmount: string,
  walletUtxosAmount: number,
};

export function WalletUtxoDescription({
  description,
  formattedWalletAmount,
  walletUtxosAmount,
}: Props) {
  const discreetModeFeature = useDiscreetModeFeature();

  return (
    <FormattedHTMLMessage
      {...description}
      values={{
        formattedWalletAmount: discreetModeFeature.hideSensitiveData(
          formattedWalletAmount
        ),
        walletUtxosAmount,
      }}
    />
  );
}
