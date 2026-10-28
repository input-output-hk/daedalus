import React, { useEffect, useRef, useState } from 'react';
import { observer } from 'mobx-react';
import { injectIntl, FormattedMessage, FormattedHTMLMessage } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import BigNumber from 'bignumber.js';
import { governanceSharedMessages } from '../../governance/_shared/governanceSharedMessages';
import BorderedBox from '../../widgets/BorderedBox';
import { messages } from './VotingPowerDelegation.messages';
import { messages as apiErrorMessages } from '../../../api/errors';
import styles from './VotingPowerDelegation.scss';
import type { Intl } from '../../../types/i18nTypes';
import WalletsDropdown from '../../widgets/forms/WalletsDropdown';
import Wallet from '../../../domains/Wallet';
import StakePool from '../../../domains/StakePool';
import { Separator } from '../../widgets/separator/Separator';
import { InitializeVPDelegationTxError } from '../../../stores/VotingStore';
import CurrentDRepSummary from './CurrentDRepSummary';
import globalMessages from '../../../i18n/global-messages';
import { sharedGovernanceMessages } from './shared-messages';
import { isSameDRep } from '../../../utils/governance/isSameDRep';
import { canonicalizeDRepId } from '../../governance/drep-directory/helpers';
import { messages as drepDirectoryMessages } from '../../governance/drep-directory/DRepDirectory';
import type { AppDRepDirectoryEntry } from '../../../stores/GovernanceStore';
import {
  drepLookupKey,
  drepOfState,
  effectiveCurrentVote,
  voteDelegationState,
} from '../../../utils/governance/voteDelegationState';
import type { DRepLookup } from '../../../utils/governance/voteDelegationState';
import DRepIdDisplay from '../../governance/_shared/DRepIdDisplay';
import DRepStatusBadge from '../../governance/_shared/DRepStatusBadge';

type Props = {
  getStakePoolById: (...args: Array<any>) => any;
  initiateTransaction?: (params: {
    chosenOption: string;
    wallet: Wallet;
  }) => Promise<
    | { success: true; fees: BigNumber }
    | { success: false; errorCode: InitializeVPDelegationTxError }
  >;
  renderConfirmationDialog?: (params: {
    chosenOption: string;
    fees: BigNumber;
    onClose: () => void;
    selectedWallet: Wallet;
  }) => React.ReactElement;
  submitTransaction: (params: {
    chosenOption: string;
    wallet: Wallet;
  }) => Promise<{ success: true } | { success: false; errorCode: string }>;
  intl: Intl;
  onExternalLinkClick: (...args: Array<any>) => any;
  stakePools: Array<StakePool>;
  wallets: Array<Wallet>;
  onSuccess: (walletId: string) => void;
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
  onFetchDRep?: (drepId: string) => Promise<AppDRepDirectoryEntry | null>;
  // What the store knows of a DRep's lookup, read while rendering.
  onLookupDRep?: (drepId: string) => DRepLookup;
  onEnsureFavorited?: (drepId: string) => void;
};

type State = {
  status:
    | 'form'
    | 'form-with-error'
    | 'form-submitted'
    | 'form-initiating-tx'
    | 'confirmation';
  fees?: BigNumber;
  selectedWalletId: string | null;
  txInitError?: TxErrorCode;
};

// The codes of the fee step, plus the one the submit step adds when the wallet
// refuses a delegation to a DRep that is not registered.
type TxErrorCode = InitializeVPDelegationTxError | 'not_found';

const mapOfTxErrorCodeToIntl: Record<
  TxErrorCode,
  (typeof messages)[keyof typeof messages]
> = {
  generic: messages.initializeTxErrorGeneric,
  same_vote: messages.transactionErrorSameVote,
  // Deliberately the same message. Having no spendable outputs and holding
  // less than the minimum are one condition to a reader: there is not enough
  // in this wallet to build the transaction, and the wallet may simply still
  // be syncing. Splitting them would name a distinction only the node makes.
  no_utxos_available: messages.initializeNotEnoughMoney,
  not_enough_money: messages.initializeNotEnoughMoney,
  // no_utxos_available while a pending transaction holds the inputs: the
  // wallet is not short of funds, they are not free yet.
  pending_transaction: apiErrorMessages.fundsHeldByPendingTransaction,
  not_found: apiErrorMessages.drepNotRegistered,
};

const SAME_VOTE_HINT_ID = 'votingPowerDelegationSameVoteHint';

function VotingPowerDelegation({
  getStakePoolById,
  submitTransaction,
  initiateTransaction,
  renderConfirmationDialog,
  initialFormState,
  intl,
  onBrowseDRepsClick,
  onEnsureFavorited,
  onCancel,
  onExternalLinkClick,
  onFetchDRep,
  onLookupDRep,
  onSuccess,
  wallets,
  stakePools,
}: Props) {
  const mounted = useRef(true);
  useEffect(
    () => () => {
      mounted.current = false;
    },
    []
  );
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

  // The choice lives here: it starts from what the Directory handed over, and
  // the form can make it without the Directory.
  const [selectedDRepId, setSelectedDRepId] = useState<string | null>(
    initialFormState?.selectedDRepId ?? null
  );
  const [pastedDRepId, setPastedDRepId] = useState('');
  const [pasteIsInvalid, setPasteIsInvalid] = useState(false);

  const selectedWallet =
    delegatableWallets.find((w) => w.id === state.selectedWalletId) ?? null;

  // The DRep whose entry the current-delegation panel shows: the one the
  // ledger delegates to, or the one an ended delegation named.
  const drepToResolve = selectedWallet
    ? drepOfState(
        voteDelegationState(selectedWallet, () => ({ status: 'idle' }))
      )
    : null;
  const drepToResolveId = drepToResolve ? drepLookupKey(drepToResolve) : null;

  const [currentDRepEntry, setCurrentDRepEntry] =
    useState<AppDRepDirectoryEntry | null>(null);
  // The lookup settling is the signal, so no timer is needed: a rejection or
  // an empty result is an answer, and only the wait before one is loading.
  const [isLookingUpDRep, setIsLookingUpDRep] = useState(false);

  useEffect(() => {
    if (drepToResolveId == null || !onFetchDRep) {
      setCurrentDRepEntry(null);
      setIsLookingUpDRep(false);
      return undefined;
    }
    let cancelled = false;
    setCurrentDRepEntry(null);
    setIsLookingUpDRep(true);
    onFetchDRep(drepToResolveId).then(
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
  }, [drepToResolveId, onFetchDRep]);

  const lookupDRep = (drepId: string): DRepLookup => {
    if (onLookupDRep) return onLookupDRep(drepId);
    if (drepId !== drepToResolveId) return { status: 'idle' };
    if (currentDRepEntry != null) {
      return { status: 'found', entry: currentDRepEntry };
    }
    return { status: isLookingUpDRep ? 'pending' : 'failed' };
  };
  const delegationState = selectedWallet
    ? voteDelegationState(selectedWallet, lookupDRep)
    : ({ kind: 'none' } as const);
  // What a new choice is compared against: the ledger's delegation, never the
  // history of a delegation that has ended.
  const currentDRep = effectiveCurrentVote(delegationState);
  const currentDRepId =
    currentDRep?.kind === 'drep' ? currentDRep.drep.raw : null;

  // Auto-favorite the current delegation DRep when a wallet is selected, so
  // DReps delegated to before the auto-favorite feature was introduced get
  // added to favorites without needing to re-select them.
  useEffect(() => {
    if (currentDRep?.kind !== 'drep' || !onEnsureFavorited) return;
    onEnsureFavorited(currentDRep.drep.cip129 ?? currentDRep.drep.raw);
  }, [currentDRepId, onEnsureFavorited]);

  // Abstain and No Confidence arrive in the same field as a DRep id, because
  // the directory offers all three as things to delegate to. They are not
  // identifiers though: there is no credential behind them, nothing to copy,
  // and no registration to report a status for.
  const selectedSentinel =
    selectedDRepId === 'abstain' || selectedDRepId === 'no_confidence'
      ? selectedDRepId
      : null;

  const [selectedDRepEntry, setSelectedDRepEntry] =
    useState<AppDRepDirectoryEntry | null>(null);

  useEffect(() => {
    // Abstain and No Confidence are not DReps: there is nothing to look up.
    if (!selectedDRepId || selectedSentinel !== null || !onFetchDRep) {
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
  }, [selectedDRepId, selectedSentinel, onFetchDRep]);

  const isSameAsCurrent =
    !!selectedDRepId && isSameDRep(selectedDRepId, currentDRep);

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
      if (!global.cip30Enabled) {
        if (!initiateTransaction)
          throw new Error('Governance preparation unavailable');
        const result = await initiateTransaction({
          chosenOption: selectedDRepId,
          wallet: selectedWallet,
        });
        if (!mounted.current) return;
        if (result.success === true) {
          setState({ ...state, fees: result.fees, status: 'confirmation' });
        } else {
          setState({
            ...state,
            txInitError: result.errorCode,
            status: 'form-with-error',
          });
        }
        return;
      }
      const result = await submitTransaction({
        chosenOption: selectedDRepId,
        wallet: selectedWallet,
      });
      if (result.success === true) {
        if (selectedSentinel === null) onEnsureFavorited(selectedDRepId);
        onSuccess(selectedWallet.id);
      } else {
        setState({
          selectedWalletId: selectedWallet.id,
          txInitError:
            result.errorCode in mapOfTxErrorCodeToIntl
              ? (result.errorCode as TxErrorCode)
              : 'generic',
          status: 'form-with-error',
        });
      }
    })();
  }, [
    onEnsureFavorited,
    onSuccess,
    selectedDRepId,
    selectedSentinel,
    selectedWallet,
    state,
    submitTransaction,
  ]);

  const displayName =
    selectedDRepEntry?.verifiedName ??
    (selectedDRepId === initialFormState?.selectedDRepId
      ? initialFormState?.selectedDRepVerifiedName
      : null) ??
    null;

  const handleUsePastedDRepId = () => {
    const canonical = canonicalizeDRepId(pastedDRepId);
    if (canonical == null) {
      setPasteIsInvalid(true);
      return;
    }
    setPasteIsInvalid(false);
    setPastedDRepId('');
    setSelectedDRepId(canonical);
  };

  const chooseAgain = () => {
    setSelectedDRepId(null);
    setPastedDRepId('');
    setPasteIsInvalid(false);
  };

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
              state={delegationState}
              lookup={
                drepToResolveId
                  ? lookupDRep(drepToResolveId)
                  : { status: 'idle' }
              }
              heldReward={selectedWallet.heldReward}
            />
          )}

          {selectedDRepId && (
            <div className={styles.selectedDRepSection}>
              <div className={styles.selectedDRepHeader}>
                <p className={styles.selectedDRepHeading}>
                  {intl.formatMessage(sharedGovernanceMessages.delegateTo)}
                </p>
                <Button
                  className={styles.selectedDRepChange}
                  label={intl.formatMessage(globalMessages.change)}
                  onClick={chooseAgain}
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
                        ? governanceSharedMessages.abstainDescription
                        : governanceSharedMessages.noConfidenceDescription
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
          {selectedWallet && !selectedDRepId && (
            <div className={styles.choices}>
              <p className={styles.choicesPrompt}>
                {intl.formatMessage(messages.choosePrompt)}
              </p>
              <div className={styles.choiceCards}>
                <div className={styles.choiceCard}>
                  <p className={styles.choiceTitle}>
                    {intl.formatMessage(sharedGovernanceMessages.abstain)}
                  </p>
                  <p className={styles.choiceDescription}>
                    {intl.formatMessage(
                      governanceSharedMessages.abstainDescription
                    )}
                  </p>
                  <Button
                    className="flat"
                    label={intl.formatMessage(
                      drepDirectoryMessages.selectOption
                    )}
                    onClick={() => setSelectedDRepId('abstain')}
                  />
                </div>
                <div className={styles.choiceCard}>
                  <p className={styles.choiceTitle}>
                    {intl.formatMessage(sharedGovernanceMessages.noConfidence)}
                  </p>
                  <p className={styles.choiceDescription}>
                    {intl.formatMessage(
                      governanceSharedMessages.noConfidenceDescription
                    )}
                  </p>
                  <Button
                    className="flat"
                    label={intl.formatMessage(
                      drepDirectoryMessages.selectOption
                    )}
                    onClick={() => setSelectedDRepId('no_confidence')}
                  />
                </div>
              </div>
              <div className={styles.pasteRow}>
                <input
                  className={styles.pasteInput}
                  type="text"
                  spellCheck={false}
                  aria-label={intl.formatMessage(
                    messages.pasteDRepIdPlaceholder
                  )}
                  placeholder={intl.formatMessage(
                    messages.pasteDRepIdPlaceholder
                  )}
                  value={pastedDRepId}
                  onChange={(event) => {
                    setPastedDRepId(event.target.value);
                    setPasteIsInvalid(false);
                  }}
                  onKeyDown={(event) => {
                    if (event.key === 'Enter') handleUsePastedDRepId();
                  }}
                />
                <Button
                  className="flat"
                  label={intl.formatMessage(messages.pasteDRepIdUse)}
                  disabled={pastedDRepId.trim() === ''}
                  onClick={handleUsePastedDRepId}
                />
              </div>
              {pasteIsInvalid && (
                <p className={styles.generalError}>
                  {intl.formatMessage(messages.pasteDRepIdInvalid)}
                </p>
              )}
            </div>
          )}
          {selectedWallet && (
            <>
              {state.status === 'form-with-error' && state.txInitError && (
                <p className={styles.generalError}>
                  <FormattedHTMLMessage
                    {...mapOfTxErrorCodeToIntl[state.txInitError]}
                    values={{ walletId: selectedWallet.id }}
                  />
                </p>
              )}

              {/* The same sentence the node returns after a submit that would
                  change nothing, and drawn the same way. It blocks the submit
                  button exactly as the server error does, so setting it in
                  quiet grey said the opposite of what it does. */}
              {isSameAsCurrent && (
                <p className={styles.generalError} id={SAME_VOTE_HINT_ID}>
                  {intl.formatMessage(messages.transactionErrorSameVote)}
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
        state.fees &&
        renderConfirmationDialog &&
        renderConfirmationDialog({
          chosenOption: selectedDRepId,
          fees: state.fees,
          selectedWallet,
          onClose: () => setState({ ...state, status: 'form' }),
        })}
    </>
  );
}

export default injectIntl(observer(VotingPowerDelegation));
