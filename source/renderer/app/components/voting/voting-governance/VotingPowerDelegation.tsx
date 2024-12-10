import React, { useEffect, useState } from 'react';
import { observer } from 'mobx-react';
import { injectIntl, FormattedMessage } from 'react-intl';
import { Input } from 'react-polymorph/lib/components/Input';
import { Button } from 'react-polymorph/lib/components/Button';
import { Link } from 'react-polymorph/lib/components/Link';

import BigNumber from 'bignumber.js';
import { Decoded, bech32 } from 'bech32';
import * as BaseEncoding from '@scure/base';
import BorderedBox from '../../widgets/BorderedBox';
import { messages } from './VotingPowerDelegation.messages';
import styles from './VotingPowerDelegation.scss';
import type { Intl } from '../../../types/i18nTypes';
import WalletsDropdown from '../../widgets/forms/WalletsDropdown';
import Wallet from '../../../domains/Wallet';
import StakePool from '../../../domains/StakePool';
import ItemsDropdown from '../../widgets/forms/ItemsDropdown';
import { Separator } from '../../widgets/separator/Separator';
import { InitializeVPDelegationTxError } from '../../../stores/VotingStore';
import { VoteType } from './types';
import { sharedGovernanceMessages } from './shared-messages';

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
};

type FormData = {
  selectedWallet: Wallet;
  selectedVoteType: VoteType;
  drepInputState: {
    dirty: boolean;
    value: string;
  };
  fees?: BigNumber;
};

type Form = Omit<FormData, 'selectedWallet'> & {
  selectedWallet: Wallet | null;
  status: 'form';
};

type FormWithError = Omit<FormData, 'status'> & {
  txInitError: InitializeVPDelegationTxError;
  status: 'form-with-error';
};

type StateFormComplete = FormData & {
  status: 'form-submitted' | 'form-initiating-tx';
};

type StateConfirmation = Omit<FormData, 'fee'> & {
  fees: BigNumber;
  status: 'confirmation';
};

type State = Form | FormWithError | StateFormComplete | StateConfirmation;

const MAX_BECH32_LENGTH_LIMIT = 1023;
const CIP_105_DREP_ID_LENGTH = 28;
const CIP_129_DREP_ID_LENGTH = 29;

const isOneOf = <T,>(target: T, options: T | T[]) =>
  (Array.isArray(options) && options.includes(target)) || target === options;

export const assertIsBech32WithPrefix = (
  target: string,
  prefix: string | string[],
  expectedDecodedLength?: number | number[]
): void => {
  let decoded: Decoded;
  try {
    decoded = bech32.decode(target, MAX_BECH32_LENGTH_LIMIT);
  } catch (error) {
    throw new Error(`expected bech32-encoded string with '${prefix}' prefix`);
  }
  if (!isOneOf(decoded.prefix, prefix)) {
    throw new Error(
      `expected bech32 prefix '${prefix}', got '${decoded.prefix}''`
    );
  }
  if (
    expectedDecodedLength &&
    !isOneOf(decoded.words.length, expectedDecodedLength)
  ) {
    throw new Error(
      `expected decoded length of '${expectedDecodedLength}', got '${decoded.words.length}'`
    );
  }
};

export const typedBech32 = <T,>(
  target: string,
  prefix: string | string[],
  expectedDecodedLength?: number | number[]
) => {
  assertIsBech32WithPrefix(target, prefix, expectedDecodedLength);
  return (target as unknown) as T;
};

export const DRepID = (value: string): string => {
  try {
    return typedBech32(value, ['drep'], 47);
  } catch {
    return typedBech32(value, ['drep', 'drep_script'], 45);
  }
};

const assertLength = (expectedLength: number | undefined, target: string) => {
  if (expectedLength && target.length !== expectedLength) {
    throw new Error(
      `expected length '${expectedLength}', got ${target.length}`
    );
  }
};

export const assertIsHexString = (
  target: string,
  expectedLength?: number
): void => {
  assertLength(expectedLength, target);
  // eslint-disable-next-line wrap-regex
  if (target.length > 0 && !/^[\da-f]+$/i.test(target)) {
    throw new Error('expected hex string');
  }
};

export const typedHex = <T,>(value: string, length?: number): T => {
  assertIsHexString(value, length);
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  return (value as any) as T;
};

export const Hash28ByteBase16 = (value: string): string =>
  typedHex<string>(value, 56);

DRepID.isValid = (value: string): boolean => {
  try {
    DRepID(value);
    return true;
  } catch {
    return false;
  }
};

export enum CredentialType {
  KeyHash = 0,
  ScriptHash = 1,
}

export type Credential = {
  type: CredentialType;
  hash: string;
};

DRepID.toCredential = (drepId: string): Credential => {
  const { words } = BaseEncoding.bech32.decode(
    drepId as any,
    MAX_BECH32_LENGTH_LIMIT
  );
  const payload = BaseEncoding.bech32.fromWords(words);

  if (
    payload.length !== CIP_105_DREP_ID_LENGTH &&
    payload.length !== CIP_129_DREP_ID_LENGTH
  ) {
    throw new Error('Invalid DRepID payload');
  }

  if (payload.length === CIP_105_DREP_ID_LENGTH) {
    const isScriptHash = drepId.includes('drep_script');

    return {
      hash: Hash28ByteBase16(Buffer.from(payload).toString('hex')),
      type: isScriptHash ? CredentialType.ScriptHash : CredentialType.KeyHash,
    };
  }

  // CIP-129
  const header = payload[0];
  const hash = payload.slice(1);
  const isDrepGovCred = (header & 0x20) === 0x20; // 0b00100000
  const isScriptHash = (header & 0x03) === 0x03; // 0b00000011

  if (!isDrepGovCred) {
    throw new Error('Invalid governance credential type');
  }

  return {
    hash: Hash28ByteBase16(Buffer.from(hash).toString('hex')),
    type: isScriptHash ? CredentialType.ScriptHash : CredentialType.KeyHash,
  };
};

DRepID.cip105FromCredential = (credential: Credential): string => {
  let prefix = 'drep';
  if (credential.type === CredentialType.ScriptHash) {
    prefix = 'drep_script';
  }

  const words = BaseEncoding.bech32.toWords(
    Buffer.from(credential.hash, 'hex')
  );

  return BaseEncoding.bech32.encode(
    prefix,
    words,
    MAX_BECH32_LENGTH_LIMIT
  ) as string;
};

DRepID.toCip105DRepID = (drepId: string): string => {
  const credential = DRepID.toCredential(drepId);
  return DRepID.cip105FromCredential(credential);
};

const isDrepIdValid = (drepId: string) =>
  DRepID.isValid(drepId) && DRepID.toCip105DRepID(drepId) === drepId;

const mapOfTxErrorCodeToIntl: Record<
  InitializeVPDelegationTxError,
  typeof messages[keyof typeof messages]
> = {
  generic: messages.initializeTxErrorGeneric,
  same_vote: messages.initializeTxErrorSameVote,
  no_utxos_available: messages.initializeNotEnoughMoney,
  not_enough_money: messages.initializeNotEnoughMoney,
};

const initialState: State = {
  status: 'form',
  selectedWallet: null,
  selectedVoteType: 'drep',
  drepInputState: {
    dirty: false,
    value: '',
  },
};

function VotingPowerDelegation({
  getStakePoolById,
  initiateTransaction,
  intl,
  onExternalLinkClick,
  renderConfirmationDialog,
  wallets,
  stakePools,
}: Props) {
  const [state, setState] = useState<State>(initialState);

  const drepInputIsValid = isDrepIdValid(state.drepInputState.value);

  const formIsValid =
    !!state.selectedWallet &&
    (state.selectedVoteType === 'drep' ? drepInputIsValid : true);

  const submitButtonDisabled =
    !formIsValid ||
    state.status === 'form-submitted' ||
    state.status === 'form-with-error' ||
    state.status === 'form-initiating-tx';

  const voteTypes: { value: VoteType; label: string }[] = [
    {
      value: 'abstain',
      label: intl.formatMessage(sharedGovernanceMessages.abstain),
    },
    {
      value: 'no_confidence',
      label: intl.formatMessage(sharedGovernanceMessages.noConfidence),
    },
    {
      value: 'drep',
      label: intl.formatMessage(sharedGovernanceMessages.delegateToDRep),
    },
  ];

  const chosenOption =
    state.selectedVoteType === 'drep'
      ? state.drepInputState.value
      : state.selectedVoteType;

  useEffect(() => {
    (async () => {
      if (state.status !== 'form-submitted') return;
      setState({
        ...state,
        status: 'form-initiating-tx',
      });
      const result = await initiateTransaction({
        chosenOption,
        wallet: state.selectedWallet,
      });

      if (result.success === true) {
        setState({
          ...state,
          fees: result.fees,
          status: 'confirmation',
        });
      } else {
        setState({
          ...state,
          txInitError: result.errorCode,
          status: 'form-with-error',
        });
      }
    })();
  }, [initiateTransaction, intl, state]);

  return (
    <>
      <div className={styles.component}>
        <BorderedBox>
          <h1 className={styles.heading}>
            {intl.formatMessage(messages.heading)}
          </h1>
          <div className={styles.info}>
            <p>
              <FormattedMessage
                {...messages.paragraph1}
                values={{
                  Link: (
                    <Link
                      className={styles.link}
                      href={intl.formatMessage(messages.paragraph1LinkUrl)}
                      label={intl.formatMessage(messages.paragraph1LinkText)}
                      onClick={(event) =>
                        onExternalLinkClick(
                          intl.formatMessage(messages.paragraph1LinkUrl),
                          event
                        )
                      }
                    />
                  ),
                }}
              />
            </p>
          </div>

          <Separator />

          <WalletsDropdown
            className={styles.walletSelect}
            // @ts-ignore ts-migrate(2322) FIXME: Type '{ className: any; label: any; numberOfStakeP... Remove this comment to see the full error message
            label={intl.formatMessage(messages.selectWalletLabel)}
            numberOfStakePools={stakePools.length}
            wallets={wallets}
            onChange={(walletId: string) => {
              const selectedWallet = wallets.find((w) => w.id === walletId);
              setState({
                ...initialState,
                selectedWallet,
              });
            }}
            placeholder={intl.formatMessage(messages.selectWalletPlaceholder)}
            value={state.selectedWallet?.id || null}
            getStakePoolById={getStakePoolById}
            disableSyncingWallets
          />

          {state.selectedWallet && (
            <ItemsDropdown
              className={styles.voteTypeSelect}
              label={intl.formatMessage(messages.selectVotingTypeLabel)}
              options={voteTypes}
              handleChange={(option) =>
                setState({
                  ...state,
                  selectedVoteType: option.value,
                  status: 'form',
                })
              }
              value={state.selectedVoteType}
            />
          )}

          {state.selectedWallet && state.selectedVoteType === 'drep' && (
            <Input
              className={styles.drepInput}
              onChange={(value) => {
                setState({
                  ...state,
                  drepInputState: {
                    dirty: true,
                    value,
                  },
                  status: 'form',
                });
              }}
              spellCheck={false}
              value={state.drepInputState.value}
              label={
                <FormattedMessage
                  {...(environment.isPreprod
                    ? messages.drepInputLabelPreprod
                    : messages.drepInputLabel)}
                  values={{
                    drepDirectoryLink: (
                      <Link
                        className={styles.link}
                        label={intl.formatMessage(
                          messages.drepInputLabelLinkText
                        )}
                        href="#"
                        onClick={(event) =>
                          onExternalLinkClick(
                            intl.formatMessage(
                              environment.isMainnet
                                ? messages.drepInputLabelLinkUrl
                                : messages.drepInputLabelLinkUrlPreview
                            ),
                            event
                          )
                        }
                      />
                    ),
                  }}
                />
              }
              placeholder={intl.formatMessage(messages.drepInputPlaceholder)}
              error={
                state.drepInputState.dirty && !drepInputIsValid
                  ? intl.formatMessage(messages.drepInputError)
                  : undefined
              }
            />
          )}

          {state.status === 'form-with-error' && (
            <p className={styles.generalError}>
              {intl.formatMessage(mapOfTxErrorCodeToIntl[state.txInitError])}
            </p>
          )}

          <Button
            label={intl.formatMessage(messages.submitLabel)}
            className={styles.voteSubmit}
            disabled={submitButtonDisabled}
            onClick={() => {
              setState({
                ...state,
                status: 'form-submitted',
              });
            }}
          />
        </BorderedBox>
      </div>
      {state.status === 'confirmation' &&
        renderConfirmationDialog({
          chosenOption,
          fees: state.fees,
          onClose: () => {
            setState({
              ...state,
              status: 'form',
            });
          },
          selectedWallet: state.selectedWallet,
        })}
    </>
  );
}

export default injectIntl(observer(VotingPowerDelegation));
