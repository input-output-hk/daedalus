import React from 'react';
import moment from 'moment';
import UndelegateWalletSuccessDialog from '../../../source/renderer/app/components/wallet/settings/UndelegateWalletSuccessDialog';
export function StakingUndelegateConfirmationResultStory({
  locale,
}: {
  locale: string;
}) {
  return (
    <UndelegateWalletSuccessDialog
      walletName="Darko's ADA"
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      slotLength={null}
      onClose={() => null}
      currentLocale={locale}
      futureEpochStartTime={moment().add(35, 'hour').toString()}
    />
  );
}
