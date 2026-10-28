import React from 'react';
import moment from 'moment';
import UndelegateWalletErrorDialog from '../../../source/renderer/app/components/wallet/settings/UndelegateWalletErrorDialog';
import LocalizableError from '../../../source/renderer/app/i18n/LocalizableError';
import { messages as apiErrorMessages } from '../../../source/renderer/app/api/errors';
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

export function StakingUndelegateRefusedBeforeApprovalStory() {
  return (
    <UndelegateWalletErrorDialog
      error={
        new LocalizableError({
          id: apiErrorMessages.withdrawalNotPossibleWithoutVote.id,
          defaultMessage:
            apiErrorMessages.withdrawalNotPossibleWithoutVote.defaultMessage,
        })
      }
      onClose={() => null}
    />
  );
}
