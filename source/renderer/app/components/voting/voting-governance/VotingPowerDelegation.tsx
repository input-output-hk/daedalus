import React, { useEffect, useState } from 'react';
import { observer } from 'mobx-react';
import { injectIntl, FormattedMessage } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';

import BigNumber from 'bignumber.js';
import BorderedBox from '../../widgets/BorderedBox';
import { messages } from './VotingPowerDelegation.messages';
import styles from './VotingPowerDelegation.scss';
import type { Intl } from '../../../types/i18nTypes';
import WalletsDropdown from '../../widgets/forms/WalletsDropdown';
import Wallet from '../../../domains/Wallet';
import StakePool from '../../../domains/StakePool';
import { Separator } from '../../widgets/separator/Separator';
import { InitializeVPDelegationTxError } from '../../../stores/VotingStore';
import CurrentDRepSummary from './CurrentDRepSummary';
import globalMessages from '../../../i18n/global-messages';
import { messages as currentDRepMessages } from './CurrentDRepSummary.messages';
import { sharedGovernanceMessages } from './shared-messages';
import { isSameDRep } from '../../../utils/governance/isSameDRep';
import type { AppDRepDirectoryEntry } from '../../../stores/GovernanceStore';
import DRepIdDisplay from '../../governance/_shared/DRepIdDisplay';
import DRepStatusBadge from '../../governance/_shared/DRepStatusBadge';

type Props = {
  getStakePoolById: (...args: Array<any>) => any;
  initiateTransaction: (params: {
    chosenOption: string;
    wallet: Wallet;
  }) => Promise<
    | { success: true; fees: BigNumber }
    | { success: false; errorCode: InitializeVPDelegationTxError }
  >;
  intl: Intl;
  onExternalLinkClick: (...args: Array<any>) => any;
  stakePools: Array<StakePool>;
  wallets: Array<Wallet>;
  renderConfirmationDialog: (params: {
    chosenOption: string;
    fees: BigNumber;
    onClose: () => void;
    selectedWallet: Wallet;
  }) => React.ReactElement;
  initialFormState?: {
    selectedWalletId?: string | null;
    selectedDRepId?: string;
    selectedDRepVerifiedName?: string | null;
    selectedDRepAnchorUrl?: string | null;
  };
  onBrowseDRepsClick: (formState: {
    selectedWalletId: string | null;
    voteType: 'drep';
  }) => void;
  onCancel: () => void;
  onFetchDRep?: (drepId: string) => Promise<AppDRepDirectoryEntry>;
  onEnsureFavorited?: (drepId: string) => void;
};

type State = {
  status:
    | 'form'
    | 'form-with-error'
    | 'form-submitted'
    | 'form-initiating-tx'
    | 'confirmation';
  selectedWalletId: string | null;
  fees?: BigNumber;
  txInitError?: InitializeVPDelegationTxError;
};

const mapOfTxErrorCodeToIntl: Record<
  InitializeVPDelegationTxError,
  (typeof messages)[keyof typeof messages]
> = {
  generic: messages.initializeTxErrorGeneric,
  same_vote: messages.initializeTxErrorSameVote,
  no_utxos_available: messages.initializeNotEnoughMoney,
  not_enough_money: messages.initializeNotEnoughMoney,
};

const SAME_VOTE_HINT_ID = 'votingPowerDelegationSameVoteHint';

function VotingPowerDelegation({
  getStakePoolById,
  initiateTransaction,
  initialFormState,
  intl,
  onBrowseDRepsClick,
  onEnsureFavorited,
  onCancel,
  onExternalLinkClick,
  onFetchDRep,
  renderConfirmationDialog,
  wallets,
  stakePools,
}: Props) {
  // A Byron wallet has no stake credential, so no delegation certificate can
  // be built for it and no amount of choosing a DRep would produce one. The
  // wallets overview already leaves them out; offering them here let someone
  // pick one and meet a transaction error instead of an explanation.
  const delegatableWallets = wallets.filter((wallet) => !wallet.isLegacy);

  const [state, setState] = useState<State>(() => {
    const { selectedWalletId } = initialFormState ?? {};
    const initialWallet =
      (selectedWalletId &&
        delegatableWallets.find((w) => w.id === selectedWalletId)) ||
      null;
    return { status: 'form', selectedWalletId: initialWallet?.id ?? null };
  });

  const selectedDRepId = initialFormState?.selectedDRepId ?? null;

  const selectedWallet =
    delegatableWallets.find((w) => w.id === state.selectedWalletId) ?? null;

  const currentDRep = selectedWallet?.currentDRep ?? null;
  const currentDRepId =
    currentDRep?.kind === 'drep' ? currentDRep.drep.raw : null;

  // Auto-favorite the current delegation DRep when a wallet is selected, so
  // DReps delegated to before the auto-favorite feature was introduced get
  // added to favorites without needing to re-select them.
  useEffect(() => {
    if (currentDRep?.kind !== 'drep' || !onEnsureFavorited) return;
    onEnsureFavorited(currentDRep.drep.cip129 ?? currentDRep.drep.raw);
  }, [currentDRepId, onEnsureFavorited]);

  const [currentDRepEntry, setCurrentDRepEntry] =
    useState<AppDRepDirectoryEntry | null>(null);
  // The lookup settling is the signal, so no timer is needed: a rejection or
  // an empty result is an answer, and only the wait before one is loading.
  const [isLookingUpDRep, setIsLookingUpDRep] = useState(false);

  useEffect(() => {
    if (currentDRep?.kind !== 'drep' || !onFetchDRep) {
      setCurrentDRepEntry(null);
      setIsLookingUpDRep(false);
      return undefined;
    }
    const drepIdToFetch = currentDRep.drep.cip129 ?? currentDRep.drep.raw;
    let cancelled = false;
    setCurrentDRepEntry(null);
    setIsLookingUpDRep(true);
    onFetchDRep(drepIdToFetch).then(
      (entry) => {
        if (cancelled) return;
        setCurrentDRepEntry(entry);
        setIsLookingUpDRep(false);
      },
      () => {
        if (cancelled) return;
        setCurrentDRepEntry(null);
        setIsLookingUpDRep(false);
      }
    );
    return () => {
      cancelled = true;
    };
  }, [currentDRepId, onFetchDRep]);

  const [selectedDRepEntry, setSelectedDRepEntry] =
    useState<AppDRepDirectoryEntry | null>(null);

  useEffect(() => {
    if (!selectedDRepId || !onFetchDRep) {
      setSelectedDRepEntry(null);
      return undefined;
    }
    let cancelled = false;
    onFetchDRep(selectedDRepId).then(
      (entry) => {
        if (!cancelled) setSelectedDRepEntry(entry);
      },
      () => {
        if (!cancelled) setSelectedDRepEntry(null);
      }
    );
    return () => {
      cancelled = true;
    };
  }, [selectedDRepId, onFetchDRep]);

  const isSameAsCurrent =
    !!selectedDRepId && isSameDRep(selectedDRepId, currentDRep);

  // Abstain and No Confidence arrive in the same field as a DRep id, because
  // the directory offers all three as things to delegate to. They are not
  // identifiers though: there is no credential behind them, nothing to copy,
  // and no registration to report a status for.
  const selectedSentinel =
    selectedDRepId === 'abstain' || selectedDRepId === 'no_confidence'
      ? selectedDRepId
      : null;

  const formIsValid = !!selectedWallet && !!selectedDRepId;

  const submitButtonDisabled =
    !formIsValid ||
    isSameAsCurrent ||
    state.status === 'form-submitted' ||
    state.status === 'form-initiating-tx';

  useEffect(() => {
    (async () => {
      if (
        state.status !== 'form-submitted' ||
        !selectedWallet ||
        !selectedDRepId
      )
        return;
      setState({ ...state, status: 'form-initiating-tx' });
      const result = await initiateTransaction({
        chosenOption: selectedDRepId,
        wallet: selectedWallet,
      });
      if (result.success === true) {
        setState({ ...state, fees: result.fees, status: 'confirmation' });
      } else {
        setState({
          ...state,
          txInitError: result.errorCode,
          status: 'form-with-error',
        });
      }
    })();
  }, [initiateTransaction, state]);

  const displayName =
    selectedDRepEntry?.verifiedName ??
    initialFormState?.selectedDRepVerifiedName ??
    null;

  const browseDReps = () =>
    onBrowseDRepsClick({
      selectedWalletId: selectedWallet?.id ?? null,
      voteType: 'drep',
    });

  return (
    <>
      <div className={styles.component}>
        <BorderedBox>
          <h1 className={styles.heading}>
            {intl.formatMessage(messages.heading)}
          </h1>
          {/* No explanation of why a delegation is needed. This screen is
              the last step of a flow that starts on the wallets overview,
              and that is where the reason now sits, in front of someone who
              has not decided yet. */}
          <Separator />

          <WalletsDropdown
            className={styles.walletSelect}
            // @ts-ignore ts-migrate(2322) FIXME: Type '{ className: any; label: any; numberOfStakeP... Remove this comment to see the full error message
            label={intl.formatMessage(messages.selectWalletLabel)}
            numberOfStakePools={stakePools.length}
            wallets={delegatableWallets}
            onChange={(walletId: string) => {
              setState({ status: 'form', selectedWalletId: walletId ?? null });
            }}
            placeholder={intl.formatMessage(messages.selectWalletPlaceholder)}
            value={selectedWallet?.id || null}
            getStakePoolById={getStakePoolById}
            disableSyncingWallets
          />

          {selectedWallet && (
            <CurrentDRepSummary
              currentDRep={currentDRep}
              drepEntry={currentDRepEntry}
              isLookingUpDRep={isLookingUpDRep}
            />
          )}

          {selectedDRepId && (
            <div className={styles.selectedDRepSection}>
              <div className={styles.selectedDRepHeader}>
                <p className={styles.selectedDRepHeading}>
                  {intl.formatMessage(messages.selectedDRepHeading)}
                </p>
                <Button
                  className={styles.selectedDRepChange}
                  label={intl.formatMessage(globalMessages.change)}
                  onClick={browseDReps}
                />
              </div>
              {selectedSentinel ? (
                <>
                  <p className={styles.selectedDRepName}>
                    {intl.formatMessage(
                      selectedSentinel === 'abstain'
                        ? sharedGovernanceMessages.abstain
                        : sharedGovernanceMessages.noConfidence
                    )}
                  </p>
                  {/* What the option does to this wallet's stake, in the
                      same words the current-delegation panel uses for a
                      wallet already set to it. Neither option is
                      self-explanatory, and this is the last screen before
                      the choice is signed. */}
                  <p className={styles.selectedOptionCaption}>
                    {intl.formatMessage(
                      selectedSentinel === 'abstain'
                        ? currentDRepMessages.abstainCaption
                        : currentDRepMessages.noConfidenceCaption
                    )}
                  </p>
                </>
              ) : (
                <>
                  {displayName && (
                    <p className={styles.selectedDRepName}>{displayName}</p>
                  )}
                  <DRepIdDisplay drepId={selectedDRepId} />
                  {selectedDRepEntry && (
                    <div className={styles.selectedDRepMeta}>
                      <DRepStatusBadge status={selectedDRepEntry.status} />
                    </div>
                  )}
                </>
              )}
            </div>
          )}
          {selectedWallet && (
            <>
              {state.status === 'form-with-error' && state.txInitError && (
                <p className={styles.generalError}>
                  {intl.formatMessage(
                    mapOfTxErrorCodeToIntl[state.txInitError]
                  )}
                </p>
              )}

              {/* The same sentence the node returns after a submit that would
                  change nothing, and drawn the same way. It blocks the submit
                  button exactly as the server error does, so setting it in
                  quiet grey said the opposite of what it does. */}
              {isSameAsCurrent && (
                <p className={styles.generalError} id={SAME_VOTE_HINT_ID}>
                  {intl.formatMessage(messages.initializeTxErrorSameVote)}
                </p>
              )}

              {/* Cancel is here whether or not anything has been chosen.
                  Without it the only control that leaves this screen was
                  Change, which goes on to the directory rather than back, so
                  someone who opened this by accident had no way out that did
                  not look like continuing. */}
              <div className={styles.submitRow}>
                {!selectedDRepId && (
                  <Button
                    className={styles.voteSubmit}
                    label={intl.formatMessage(messages.browseDRepsButton)}
                    onClick={browseDReps}
                  />
                )}
                {selectedDRepId && (
                  <Button
                    label={intl.formatMessage(globalMessages.submit)}
                    className={styles.voteSubmit}
                    disabled={submitButtonDisabled}
                    aria-describedby={
                      isSameAsCurrent ? SAME_VOTE_HINT_ID : undefined
                    }
                    onClick={() => {
                      setState({ ...state, status: 'form-submitted' });
                    }}
                  />
                )}
                <Button
                  className={`flat ${styles.voteCancel}`}
                  label={intl.formatMessage(globalMessages.cancel)}
                  onClick={onCancel}
                />
              </div>
            </>
          )}
        </BorderedBox>
      </div>
      {state.status === 'confirmation' &&
        selectedWallet &&
        selectedDRepId &&
        renderConfirmationDialog({
          chosenOption: selectedDRepId,
          fees: state.fees!,
          onClose: () => {
            setState({ ...state, status: 'form' });
          },
          selectedWallet,
        })}
    </>
  );
}

export default injectIntl(observer(VotingPowerDelegation));
