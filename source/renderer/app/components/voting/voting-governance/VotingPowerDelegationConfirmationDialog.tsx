import React, { useEffect, useState } from 'react';
import BigNumber from 'bignumber.js';
import { injectIntl } from 'react-intl';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import Dialog from '../../widgets/Dialog';
import { formattedWalletAmount } from '../../../utils/formatters';
import Wallet, {
  HwDeviceStatus,
  HwDeviceStatuses,
} from '../../../domains/Wallet';
import HardwareWalletStatus from '../../hardware-wallet/HardwareWalletStatus';
import styles from './VotingPowerDelegationConfirmationDialog.scss';
import { DelegateVotesError } from '../../../stores/VotingStore';
import type { Intl, ReactIntlMessage } from '../../../types/i18nTypes';
import globalMessages from '../../../i18n/global-messages';
import { messages } from './VotingPowerDelegationConfirmationDialog.messages';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import { VoteType } from './types';
import { sharedGovernanceMessages } from './shared-messages';
import { messages as apiErrorMessage } from '../../../api/errors';
import type { DRepIdentity } from '../../../../../common/types/governance.types';

const mapOfTxErrorCodeToIntl: Record<
  DelegateVotesError,
  (typeof messages)[keyof typeof messages]
> = {
  generic: messages.errorGeneric,
  wrong_encryption_passphrase: apiErrorMessage.wrongEncryptionPassphrase,
};

// Reached only for the two sentinels: a DRep delegation renders its name and
// its identifier instead, neither of which is a translated label.
const mapVoteToIntlMessage = (vote: VoteType | string): ReactIntlMessage =>
  vote === 'abstain'
    ? sharedGovernanceMessages.abstain
    : sharedGovernanceMessages.noConfidence;

/**
 * The verified name plus the host that served the hash-matched bytes. One
 * object, not two props, so `filterLogData`'s existing `verifiedName` key
 * redacts the host with it at any depth.
 */
export type VerifiedDRepNameSource = {
  host: string;
  name: string;
};

export type VotingPowerDelegationConfirmationDialogState =
  | {
      error?: DelegateVotesError;
      passphrase: string;
      status: 'awaiting';
    }
  | {
      passphrase: string;
      status: 'confirmed';
    }
  | { status: 'submitting' };

type VotingPowerDelegationConfirmationDialogProps = {
  chosenOption: string;
  drepIdentity: DRepIdentity | null;
  fees: BigNumber;
  hwDeviceStatus: HwDeviceStatus;
  intl: Intl;
  isTrezor: boolean;
  onClose: () => void;
  onExternalLinkClick: (...args: Array<any>) => any;
  onSubmit: (
    passphrase?: string
  ) => Promise<
    { success: true } | { success: false; errorCode: DelegateVotesError }
  >;
  redirectToWallet: (walletId: string) => void;
  selectedWallet: Wallet;
  verifiedName: VerifiedDRepNameSource | null;
};

function VotingPowerDelegationConfirmationDialog({
  chosenOption,
  drepIdentity,
  fees,
  hwDeviceStatus,
  intl,
  isTrezor,
  onClose,
  onExternalLinkClick,
  onSubmit,
  redirectToWallet,
  selectedWallet,
  verifiedName,
}: VotingPowerDelegationConfirmationDialogProps) {
  const [state, setState] =
    useState<VotingPowerDelegationConfirmationDialogState>({
      passphrase: '',
      status: 'awaiting',
    });

  useEffect(() => {
    (async () => {
      if (state.status !== 'confirmed') return;

      const { passphrase, ...restState } = state;
      setState({
        ...restState,
        status: 'submitting',
      });

      const result = await onSubmit(passphrase);

      if (result.success === true) {
        redirectToWallet(selectedWallet.id);
        return;
      }

      setState({
        ...state,
        error: result.errorCode,
        status: 'awaiting',
      });
    })();
  }, [intl, onSubmit, redirectToWallet, state]);

  // Keyed on the vote kind, not on a successful decode: an id the decoder
  // rejects still renders verbatim rather than as a vote label.
  const isSentinelVote =
    chosenOption === 'abstain' || chosenOption === 'no_confidence';

  const certificateJson = JSON.stringify(
    {
      vote: isSentinelVote
        ? { type: chosenOption }
        : { type: 'drep', id: drepIdentity?.credentialHex ?? chosenOption },
    },
    null,
    2
  );

  const confirmButtonLabel =
    state.status === 'awaiting' ? (
      intl.formatMessage(globalMessages.confirm)
    ) : (
      <LoadingSpinner />
    );

  return (
    <Dialog
      title={intl.formatMessage(messages.title)}
      actions={[
        {
          label: intl.formatMessage(globalMessages.cancel),
          onClick: onClose,
          disabled: state.status !== 'awaiting',
        },
        {
          label: confirmButtonLabel,
          onClick: () => {
            setState({
              passphrase: ('passphrase' in state && state.passphrase) || '',
              status: 'confirmed',
            });
          },
          primary: true,
          disabled:
            state.status !== 'awaiting' ||
            (selectedWallet.isHardwareWallet
              ? hwDeviceStatus !==
                HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED
              : !state.passphrase),
        },
      ]}
    >
      <div className={styles.content}>
        {/* One heading for every kind of delegation, named the way the form
            that proposed it named it. Abstain and No Confidence are targets
            of a delegation certificate like any DRep, so they are shown the
            same way rather than under a label of their own. */}
        <p className={styles.paragraphTitle}>
          {intl.formatMessage(sharedGovernanceMessages.delegateTo)}
        </p>
        {isSentinelVote ? (
          <p className={styles.paragraphValue}>
            {intl.formatMessage(mapVoteToIntlMessage(chosenOption))}
          </p>
        ) : (
          <>
            {/* Only the hash-guarded verified projection reaches here; an
                unverified anchor name never renders on a signing surface. */}
            {verifiedName && (
              <p className={styles.delegateToName}>{verifiedName.name}</p>
            )}
            <p className={styles.paragraphValue}>
              {/* Rendered untouched: this string must stay byte-equal to
                  chosenOption and the delegateVotes dRepId.

                  No CIP-105 form here. It is offered on the directory and the
                  detail view, where someone is cross-referencing an explorer
                  that may still print the deprecated prefix. This screen is
                  where they commit, and a second identifier for the same DRep
                  is one more thing to check and nothing to act on. */}
              <code className={styles.drepIdValue}>
                {drepIdentity?.raw ?? chosenOption}
              </code>
            </p>
          </>
        )}

        <p className={styles.paragraphTitle}>
          {intl.formatMessage(messages.delegationCertificate)}
        </p>
        <pre className={styles.certificateValue}>{certificateJson}</pre>

        <p className={styles.paragraphTitle}>
          {intl.formatMessage(globalMessages.transactionFee)}
        </p>
        <p className={styles.paragraphValue}>{formattedWalletAmount(fees)}</p>

        {selectedWallet.isHardwareWallet ? (
          <HardwareWalletStatus
            hwDeviceStatus={hwDeviceStatus}
            walletName={selectedWallet.name}
            isTrezor={isTrezor}
            onExternalLinkClick={onExternalLinkClick}
          />
        ) : (
          <Input
            autoFocus
            value={state.status === 'awaiting' ? state.passphrase : ''}
            onChange={(passphrase) => {
              if (state.status !== 'awaiting') return;
              setState({
                ...state,
                passphrase,
              });
            }}
            disabled={state.status !== 'awaiting'}
            type={'password'}
            label={intl.formatMessage(globalMessages.spendingPassword)}
            skin={InputSkin}
          />
        )}

        {'error' in state && (
          <p className={styles.error}>
            {intl.formatMessage(mapOfTxErrorCodeToIntl[state.error])}
          </p>
        )}
      </div>
    </Dialog>
  );
}

export default injectIntl(VotingPowerDelegationConfirmationDialog);
