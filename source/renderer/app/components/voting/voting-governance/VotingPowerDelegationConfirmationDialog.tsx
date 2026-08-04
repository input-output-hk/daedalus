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
import DRepSourceLabel from '../../governance/_shared/DRepSourceLabel';
import styles from './VotingPowerDelegationConfirmationDialog.scss';
import { DelegateVotesError } from '../../../stores/VotingStore';
import type { Intl, ReactIntlMessage } from '../../../types/i18nTypes';
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

const mapVoteToIntlMessage = (vote: VoteType | string): ReactIntlMessage => {
  switch (vote) {
    case 'abstain':
      return sharedGovernanceMessages.abstain;
    case 'no_confidence':
      return sharedGovernanceMessages.noConfidence;
    default:
      return sharedGovernanceMessages.delegateToDRep;
  }
};

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

  const confirmButtonLabel =
    state.status === 'awaiting' ? (
      intl.formatMessage(messages.buttonConfirm)
    ) : (
      <LoadingSpinner />
    );

  return (
    <Dialog
      title={intl.formatMessage(messages.title)}
      actions={[
        {
          label: intl.formatMessage(messages.buttonCancel),
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
        {!isSentinelVote ? (
          <>
            {verifiedName && (
              <>
                {/* Only the hash-guarded verified projection reaches here; an
                    unverified anchor name never renders on a signing surface. */}
                <p className={styles.paragraphTitle}>
                  {intl.formatMessage(messages.verifiedName)}
                </p>
                <p className={styles.paragraphValue}>{verifiedName.name}</p>
              </>
            )}
            <p className={styles.paragraphTitle}>
              {intl.formatMessage(messages.drepId)}
            </p>
            <p className={styles.paragraphValue}>
              {/* Rendered untouched: this string must stay byte-equal to
                  chosenOption and the delegateVotes dRepId. */}
              <code className={styles.drepIdValue}>
                {drepIdentity?.raw ?? chosenOption}
              </code>
            </p>
            {drepIdentity?.cip105 &&
              drepIdentity.cip105 !== drepIdentity.raw && (
                <>
                  <p className={styles.paragraphTitle}>
                    {intl.formatMessage(messages.drepIdCip105)}
                  </p>
                  <p className={styles.paragraphValue}>
                    <code className={styles.drepIdValue}>
                      {drepIdentity.cip105}
                    </code>
                  </p>
                </>
              )}
            {drepIdentity?.credentialHex && (
              <>
                <p className={styles.paragraphTitle}>
                  {intl.formatMessage(messages.signedPayload)}
                </p>
                <p className={styles.paragraphValue}>
                  <code className={styles.drepIdValue}>
                    {`{"vote":{"type":"drep","id":"${drepIdentity.credentialHex}"}}`}
                  </code>
                </p>
              </>
            )}
            {(drepIdentity || verifiedName) && (
              <p className={styles.paragraphValue}>
                <DRepSourceLabel source="on-chain" />
                {verifiedName && (
                  <>
                    {' · '}
                    {intl.formatMessage(messages.verifiedNameSource)}{' '}
                    <DRepSourceLabel
                      source="verified-off-chain"
                      host={verifiedName.host}
                    />
                  </>
                )}
              </p>
            )}
          </>
        ) : (
          <>
            <p className={styles.paragraphTitle}>
              {intl.formatMessage(messages.vote)}
            </p>
            <p className={styles.paragraphValue}>
              {intl.formatMessage(mapVoteToIntlMessage(chosenOption))}
            </p>
          </>
        )}

        <p className={styles.paragraphTitle}>
          {intl.formatMessage(messages.fee)}
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
            label={intl.formatMessage(messages.password)}
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
