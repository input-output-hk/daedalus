import React from 'react';
import { FormattedHTMLMessage } from 'react-intl';
import { observer } from 'mobx-react';
import { useDiscreetModeFeature } from '../../../features/discreet-mode';
import type { ReactIntlMessage } from '../../../types/i18nTypes';

type Props = {
  description: ReactIntlMessage;
  formattedWalletAmount: string;
  walletUtxosAmount: number;
};

function WalletUtxoDescriptionComponent({
  description,
  formattedWalletAmount,
  walletUtxosAmount,
}: Props) {
  const discreetModeFeature = useDiscreetModeFeature();
  return (
    <FormattedHTMLMessage
      {...description}
      values={{
        formattedWalletAmount: discreetModeFeature.discreetValue({
          value: formattedWalletAmount,
        }),
        walletUtxosAmount,
      }}
    />
  );
}

export const WalletUtxoDescription = observer(WalletUtxoDescriptionComponent);
