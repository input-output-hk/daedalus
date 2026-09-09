import BigNumber from 'bignumber.js';
import classnames from 'classnames';
import React, { useEffect, useMemo, useRef, useState } from 'react';
import CopyToClipboard from 'react-copy-to-clipboard';
import { injectIntl } from 'react-intl';
import type {
  TransactionReviewAsset,
  TransactionReviewEntry,
  TransactionReviewValue,
} from '../../../../common/transactions/reviewDisplay';
import { LOVELACES_PER_ADA } from '../../config/numbersConfig';
import type { Intl } from '../../types/i18nTypes';
import { formattedWalletAmount } from '../../utils/formatters';
import AssetContent from '../assets/AssetContent';
import { CollapsibleSection } from '../widgets/collapsible-section/CollapsibleSection';
import Dialog from '../widgets/Dialog';
import { MonospaceTextBlock } from '../widgets/monospace-text-block/MonospaceTextBlock';
import { messages } from './TransactionApprovalDialog.messages';
import type {
  TransactionApprovalDialogProps,
  TransactionApprovalItem,
  TransactionApprovalRequest,
} from './TransactionApprovalDialog.types';
import styles from './TransactionApprovalDialog.scss';

const unsafeText = /[\p{Cc}\u202a-\u202e\u2066-\u2069]/gu;
const safeText = (value: string) => value.replace(unsafeText, '�');
const preview = (value: string) =>
  value.length > 24 ? `${value.slice(0, 12)}…${value.slice(-8)}` : value;
const ada = (lovelace: string) =>
  formattedWalletAmount(
    new BigNumber(lovelace).dividedBy(LOVELACES_PER_ADA),
    true,
    true
  );
const signedAda = (lovelace: string, sign: '+' | '−') =>
  `${sign}${ada(new BigNumber(lovelace).absoluteValue().toFixed(0))}`;

const certificateNames: Record<number, string> = {
  0: 'Stake registration',
  1: 'Stake deregistration',
  2: 'Stake delegation',
  3: 'Pool registration',
  4: 'Pool retirement',
  7: 'Stake registration',
  8: 'Stake deregistration',
  9: 'Voting delegation',
  10: 'Stake and voting delegation',
  11: 'Stake registration and delegation',
  12: 'Stake registration and voting delegation',
  13: 'Stake registration and stake/voting delegation',
  14: 'Committee hot-key authorization',
  15: 'Committee resignation',
  16: 'DRep registration',
  17: 'DRep deregistration',
  18: 'DRep update',
};

const entryMessages = {
  input: messages.input,
  output: messages.output,
  'reference-input': messages.referenceInput,
  'collateral-input': messages.collateralInput,
  'collateral-return': messages.collateralReturnEntry,
};

const operationMessages = {
  sign: messages.sign,
  submit: messages.submit,
  'sign-and-submit': messages.signAndSend,
};

type Props = TransactionApprovalDialogProps & { intl: Intl };

function AssetRow({
  asset,
  props,
  sign,
}: {
  asset: TransactionReviewAsset;
  props: Props;
  sign?: '+' | '−';
}) {
  const descriptor = props.assetDetails[`${asset.policyId}${asset.assetName}`];
  const decimals = descriptor?.decimals;
  const validDecimals =
    Number.isInteger(decimals) &&
    Number(decimals) >= 0 &&
    Number(decimals) <= 20;
  const quantity = new BigNumber(asset.quantity).absoluteValue();
  const name =
    safeText(
      descriptor?.metadata?.name || descriptor?.metadata?.ticker || ''
    ) || props.intl.formatMessage(messages.unnamed);
  const amount = validDecimals
    ? quantity.shiftedBy(-Number(decimals)).toFormat(Number(decimals))
    : props.intl.formatMessage(messages.baseUnits, {
        value: quantity.toFixed(0),
      });
  const details = descriptor || {
    assetName: asset.assetName,
    fingerprint: asset.fingerprint,
    policyId: asset.policyId,
    uniqueId: `${asset.policyId}${asset.assetName}`,
  };
  const signedAmount = `${sign || ''}${amount}`;
  const rawQuantity = `${sign || ''}${quantity.toFixed(0)}`;
  return (
    <details
      className={classnames(styles.assetRow, {
        [styles.outgoing]: sign === '−',
        [styles.incoming]: sign === '+',
      })}
    >
      <summary
        aria-label={`${props.intl.formatMessage(messages.assetDetails)}: ${
          asset.fingerprint
        }`}
      >
        <span className={styles.assetAmount}>{signedAmount}</span>
        <bdi>{name}</bdi>
        <code>{preview(asset.fingerprint)}</code>
      </summary>
      <div className={styles.assetDetails}>
        <AssetContent asset={details} displayMode="review" />
        <p>
          {props.intl.formatMessage(messages.rawUnits, {
            value: rawQuantity,
          })}
        </p>
      </div>
    </details>
  );
}

function ValueRows({
  value,
  props,
  sign,
  limitAssets = false,
}: {
  value: TransactionReviewValue | null;
  props: Props;
  sign?: '+' | '−';
  limitAssets?: boolean;
}) {
  if (!value)
    return <p>{props.intl.formatMessage(messages.valueUnavailable)}</p>;
  const visibleAssets = limitAssets ? value.assets.slice(0, 2) : value.assets;
  const hiddenAssets = limitAssets ? value.assets.slice(2) : [];
  return (
    <div className={styles.valueRows}>
      <div
        className={classnames(styles.amount, {
          [styles.outgoingText]: sign === '−',
          [styles.incomingText]: sign === '+',
        })}
      >
        {sign || ''}
        {ada(value.coin)}
      </div>
      {visibleAssets.map((asset) => (
        <AssetRow
          key={`${asset.policyId}:${asset.assetName}`}
          asset={asset}
          props={props}
          sign={sign}
        />
      ))}
      {hiddenAssets.length > 0 && (
        <details className={styles.moreAssets}>
          <summary>
            {props.intl.formatMessage(messages.moreAssets, {
              count: hiddenAssets.length,
            })}
          </summary>
          {hiddenAssets.map((asset) => (
            <AssetRow
              key={`${asset.policyId}:${asset.assetName}`}
              asset={asset}
              props={props}
              sign={sign}
            />
          ))}
        </details>
      )}
    </div>
  );
}

function ImpactCard({
  direction,
  value,
  props,
}: {
  direction: 'leaving' | 'coming';
  value: TransactionReviewValue | null;
  props: Props;
}) {
  const leaving = direction === 'leaving';
  if (!value)
    return (
      <div
        className={`${styles.impactCard} ${
          leaving ? styles.outgoing : styles.incoming
        }`}
      >
        <h3>{props.intl.formatMessage(messages[direction])}</h3>
        <p>{props.intl.formatMessage(messages.unavailable)}</p>
      </div>
    );
  const coinMatches = leaving
    ? new BigNumber(value.coin).isNegative()
    : new BigNumber(value.coin).isPositive();
  const assets = value.assets.filter(({ quantity }) =>
    leaving
      ? new BigNumber(quantity).isNegative()
      : new BigNumber(quantity).isPositive()
  );
  return (
    <div
      className={`${styles.impactCard} ${
        leaving ? styles.outgoing : styles.incoming
      }`}
    >
      <h3>{props.intl.formatMessage(messages[direction])}</h3>
      {!coinMatches && assets.length === 0 && (
        <p>
          {props.intl.formatMessage(
            leaving ? messages.noneLeaving : messages.noneComing
          )}
        </p>
      )}
      {coinMatches && (
        <div className={styles.primaryAmount}>
          {signedAda(value.coin, leaving ? '−' : '+')}
        </div>
      )}
      {assets.map((asset) => (
        <AssetRow
          key={`${asset.policyId}:${asset.assetName}`}
          asset={asset}
          props={props}
          sign={leaving ? '−' : '+'}
        />
      ))}
    </div>
  );
}

function AddressDisclosure({
  address,
  props,
}: {
  address: string;
  props: Props;
}) {
  const split = Math.max(0, address.length - 8);
  return (
    <details className={styles.address}>
      <summary>
        <code>{address.slice(0, split)}</code>
        <strong>{address.slice(split)}</strong>
      </summary>
      <code className={styles.fullText}>{address}</code>
      <CopyToClipboard text={address}>
        <button type="button" className={styles.copy}>
          {props.intl.formatMessage(messages.copyAddress)}
        </button>
      </CopyToClipboard>
    </details>
  );
}

function EntryCard({
  entry,
  props,
}: {
  entry: TransactionReviewEntry;
  props: Props;
}) {
  const point = entry.outpoint
    ? `${entry.outpoint.transactionId}#${entry.outpoint.index}`
    : null;
  const message = entryMessages[entry.role];
  let sign: '+' | '−' | undefined;
  if (entry.role === 'input') sign = '−';
  if (entry.role === 'output') sign = '+';
  let ownershipMessage = messages.ownershipUnknown;
  if (entry.ownership === 'wallet') {
    ownershipMessage =
      entry.role === 'output' ? messages.returnedToWallet : messages.thisWallet;
  } else if (entry.ownership === 'other') {
    ownershipMessage = messages.other;
  }
  return (
    <article className={styles.entryCard}>
      <header>
        <h3>
          {props.intl.formatMessage(message, {
            value: entry.position + 1,
          })}
        </h3>
        <div className={styles.badges}>
          <span className={styles.badge}>
            {props.intl.formatMessage(ownershipMessage)}
          </span>
          {entry.control === 'script' && (
            <span className={styles.badge}>
              {props.intl.formatMessage(messages.script)}
            </span>
          )}
        </div>
      </header>
      <ValueRows
        value={entry.value}
        props={props}
        sign={sign}
        limitAssets={entry.role === 'input' || entry.role === 'output'}
      />
      {entry.address && (
        <AddressDisclosure address={entry.address} props={props} />
      )}
      {point && (
        <div className={styles.identity}>
          <span>{props.intl.formatMessage(messages.outpoint)}</span>
          <code>{preview(point)}</code>
          <details>
            <summary>{props.intl.formatMessage(messages.fullOutpoint)}</summary>
            <code className={styles.fullText}>{point}</code>
            <CopyToClipboard text={point}>
              <button type="button" className={styles.copy}>
                {props.intl.formatMessage(messages.copyOutpoint)}
              </button>
            </CopyToClipboard>
          </details>
        </div>
      )}
      {entry.hasDatum && <p>{props.intl.formatMessage(messages.datum)}</p>}
      {entry.hasReferenceScript && (
        <p>{props.intl.formatMessage(messages.referenceScript)}</p>
      )}
    </article>
  );
}

function EntrySection({
  entries,
  title,
  props,
  initiallyOpen = false,
  warning = false,
}: {
  entries: readonly TransactionReviewEntry[];
  title: string;
  props: Props;
  initiallyOpen?: boolean;
  warning?: boolean;
}) {
  return entries.length ? (
    <details
      className={`${styles.entrySection} ${warning ? styles.warningGroup : ''}`}
      open={initiallyOpen}
    >
      <summary>
        {props.intl.formatMessage(messages.entrySection, {
          title,
          count: entries.length,
        })}
      </summary>
      <div className={styles.entryGrid}>
        {entries.map((entry) => (
          <EntryCard
            key={`${entry.role}:${entry.position}`}
            entry={entry}
            props={props}
          />
        ))}
      </div>
    </details>
  ) : null;
}

function OtherActions({
  item,
  props,
}: {
  item: TransactionApprovalItem;
  props: Props;
}) {
  const { display } = item;
  const present =
    display.mint.length ||
    display.withdrawals.length ||
    display.certificates.length ||
    display.votes.length ||
    display.proposalCount ||
    display.donation !== null;
  if (!present) return null;
  return (
    <section>
      <h2>{props.intl.formatMessage(messages.otherActions)}</h2>
      {display.mint.length > 0 && (
        <div>
          <h3>{props.intl.formatMessage(messages.minting)}</h3>
          {display.mint.map((asset) => (
            <AssetRow
              key={`${asset.policyId}:${asset.assetName}`}
              asset={asset}
              props={props}
              sign={new BigNumber(asset.quantity).isNegative() ? '−' : '+'}
            />
          ))}
        </div>
      )}
      {display.withdrawals.length > 0 && (
        <div>
          <h3>{props.intl.formatMessage(messages.withdrawals)}</h3>
          {display.withdrawals.map((withdrawal) => (
            <p key={withdrawal.account}>
              {ada(withdrawal.coin)} · <code>{withdrawal.account}</code> ·{' '}
              {withdrawal.ownership}
            </p>
          ))}
        </div>
      )}
      {display.certificates.length > 0 && (
        <div>
          <h3>{props.intl.formatMessage(messages.certificates)}</h3>
          {display.certificates.map((certificate, index) => (
            <div key={index} className={styles.actionCard}>
              <strong>
                {certificateNames[certificate.kind] ||
                  `Certificate ${certificate.kind}`}
              </strong>
              {certificate.poolId && <code>{certificate.poolId}</code>}
              {certificate.credentialIdentities.map((value) => (
                <code key={value}>{value}</code>
              ))}
              {certificate.targetCredentialIdentities.map((value) => (
                <code key={value}>{value}</code>
              ))}
            </div>
          ))}
        </div>
      )}
      {(display.votes.length > 0 || display.proposalCount > 0) && (
        <div>
          <h3>{props.intl.formatMessage(messages.governance)}</h3>
          {display.votes.map((vote) => (
            <div key={vote.voter} className={styles.actionCard}>
              <code>{vote.voter}</code>
              {vote.actionIds.map((id) => (
                <code key={id}>{id}</code>
              ))}
            </div>
          ))}
          {display.proposalCount > 0 && (
            <p>{display.proposalCount} proposal(s)</p>
          )}
        </div>
      )}
      {display.donation !== null && <p>Donation: {ada(display.donation)}</p>}
    </section>
  );
}

function ItemReview({
  item,
  props,
}: {
  item: TransactionApprovalItem;
  props: Props;
}) {
  const { display } = item;
  const inputs = display.entries.filter(({ role }) => role === 'input');
  const outputs = display.entries.filter(({ role }) => role === 'output');
  const references = display.entries.filter(
    ({ role }) => role === 'reference-input'
  );
  const collateralInputs = display.entries.filter(
    ({ role }) => role === 'collateral-input'
  );
  const collateralReturns = display.entries.filter(
    ({ role }) => role === 'collateral-return'
  );
  return (
    <article className={styles.item}>
      {props.request.items.length > 1 && (
        <h2>
          {props.intl.formatMessage(messages.item, {
            current: item.index + 1,
            total: props.request.items.length,
          })}
        </h2>
      )}
      {item.dependencies.map((dependency) => (
        <p
          className={styles.notice}
          key={`${dependency.inputRole}:${dependency.outpoint.transactionId}:${dependency.outpoint.index}`}
        >
          {props.intl.formatMessage(messages.dependencies, {
            value: (dependency.sourceTransactionIndex || 0) + 1,
          })}
        </p>
      ))}
      {item.conflicts.map((conflict) => (
        <p
          className={styles.warning}
          key={`${conflict.inputRole}:${conflict.outpoint.transactionId}:${conflict.outpoint.index}`}
        >
          {props.intl.formatMessage(messages.conflict, {
            value: conflict.earlierTransactionIndex + 1,
          })}
        </p>
      ))}
      {!item.approvable && (
        <div className={styles.refusal} role="alert">
          <strong>{props.intl.formatMessage(messages.refusal)}</strong>
          {item.refusalReasons.map((reason) => (
            <code key={reason}>{reason}</code>
          ))}
        </div>
      )}
      <div className={styles.riskStrip}>
        <div className={styles.fee}>
          <span>{props.intl.formatMessage(messages.fee)}</span>
          <strong>−{ada(display.fee)}</strong>
        </div>
        {display.maximumCollateralLoss && (
          <div className={styles.collateralRisk}>
            <span>{props.intl.formatMessage(messages.collateral)}</span>
            <strong>
              {props.intl.formatMessage(messages.upTo, {
                value: ada(display.maximumCollateralLoss.coin),
              })}
            </strong>
            <small>{props.intl.formatMessage(messages.collateralHelp)}</small>
          </div>
        )}
      </div>
      <section>
        <h2>{props.intl.formatMessage(messages.changes)}</h2>
        <p>{props.intl.formatMessage(messages.changeHelp)}</p>
        <div className={styles.impactGrid}>
          <ImpactCard
            direction="leaving"
            value={display.walletChange}
            props={props}
          />
          <ImpactCard
            direction="coming"
            value={display.walletChange}
            props={props}
          />
        </div>
        {display.walletInputs && display.walletOutputs && (
          <details className={styles.calculation}>
            <summary>{props.intl.formatMessage(messages.calculation)}</summary>
            <p>
              {props.intl.formatMessage(messages.gross, {
                inputs: ada(display.walletInputs.coin),
                outputs: ada(display.walletOutputs.coin),
              })}
            </p>
            <p>{props.intl.formatMessage(messages.returned)}</p>
          </details>
        )}
      </section>
      <OtherActions item={item} props={props} />
      <p className={styles.grossFlowHelp}>
        {props.intl.formatMessage(messages.grossFlowHelp)}
      </p>
      <div className={styles.columns}>
        <div className={styles.entryStack}>
          <EntrySection
            entries={inputs}
            title={props.intl.formatMessage(messages.inputs)}
            props={props}
            initiallyOpen
          />
          <EntrySection
            entries={references}
            title={props.intl.formatMessage(messages.referenceInputs)}
            props={props}
          />
          <EntrySection
            entries={collateralInputs}
            title={props.intl.formatMessage(messages.collateralInputs)}
            props={props}
            warning
          />
        </div>
        <div className={styles.entryStack}>
          <EntrySection
            entries={outputs}
            title={props.intl.formatMessage(messages.outputs)}
            props={props}
            initiallyOpen
          />
          <EntrySection
            entries={collateralReturns}
            title={props.intl.formatMessage(messages.collateralReturn)}
            props={props}
            warning
          />
        </div>
      </div>
      <CollapsibleSection
        header={props.intl.formatMessage(messages.technical)}
        contentId={`transaction-technical-${props.request.requestId}-${item.index}`}
      >
        <div className={styles.technical}>
          <p className={styles.checked}>
            {props.intl.formatMessage(messages.checked)}
          </p>
          {item.effects.map((effect) => (
            <details key={effect.index}>
              <summary>
                {effect.index + 1}. {effect.kind}
              </summary>
              <MonospaceTextBlock>{effect.value}</MonospaceTextBlock>
            </details>
          ))}
          {item.evidence.kind === 'exact-cbor' ? (
            <>
              <h3>Transaction hash</h3>
              <MonospaceTextBlock>
                {item.evidence.review.transactionId}
              </MonospaceTextBlock>
              <h3>Exact body CBOR</h3>
              <MonospaceTextBlock>
                {item.evidence.review.bodyCbor}
              </MonospaceTextBlock>
              {item.evidence.review.mode === 'submit' && (
                <>
                  <h3>Exact submitted envelope CBOR</h3>
                  <MonospaceTextBlock>
                    {item.evidence.review.fullCbor}
                  </MonospaceTextBlock>
                </>
              )}
            </>
          ) : (
            <>
              <h3>Plan digest</h3>
              <MonospaceTextBlock>
                {item.evidence.planDigest}
              </MonospaceTextBlock>
              <h3>Bound plan CBOR</h3>
              <MonospaceTextBlock>{item.evidence.planCbor}</MonospaceTextBlock>
            </>
          )}
        </div>
      </CollapsibleSection>
    </article>
  );
}

function ResultSummary({
  result,
  request,
  receipts = [],
  onViewTransaction,
  intl,
}: Pick<Props, 'request' | 'receipts' | 'onViewTransaction' | 'intl'> & {
  result: NonNullable<Props['result']>;
}) {
  const errorCode =
    result.status === 'rejected' || result.status === 'partial'
      ? result.errorCode
      : undefined;
  const deviceRejected = errorCode === 'TxSignError.UserDeclined';
  const cancelled = errorCode === 'cancelled' || errorCode === 'user_declined';
  const failedItem =
    result.status === 'partial' ? result.failedIndex + 1 : undefined;
  const transactionIds =
    result.status === 'rejected'
      ? receipts.map(({ id }) => id)
      : result.transactionIds;
  const allKnown =
    transactionIds.length > 0 && receipts.length === transactionIds.length;
  let title = messages.rejectedTitle;
  let message = messages.rejectedMessage;
  let variant = styles.resultFailure;
  if (result.status === 'signed') {
    title = messages.signedTitle;
    message = messages.signedMessage;
    variant = styles.resultPending;
  } else if (result.status === 'partial') {
    title = messages.partialTitle;
    message = messages.partialMessage;
    if (cancelled) message = messages.partialCancelledMessage;
    else if (deviceRejected) message = messages.partialDeviceRejectedMessage;
  } else if (allKnown && receipts.every(({ state }) => state === 'in_ledger')) {
    title = messages.confirmedTitle;
    message = messages.confirmedMessage;
    variant = styles.resultSuccess;
  } else if (
    errorCode === 'expired' ||
    (allKnown && receipts.every(({ state }) => state === 'expired'))
  ) {
    title = messages.expiredTitle;
    message = messages.expiredMessage;
  } else if (allKnown && receipts.every(({ state }) => state === 'failed')) {
    title = messages.failedTitle;
    message = messages.failedMessage;
  } else if (
    receipts.some(({ state }) => state === 'expired' || state === 'failed')
  ) {
    title = messages.updatedTitle;
    message = messages.updatedMessage;
    variant = styles.resultUnknown;
  } else if (
    (result.status === 'submitted' &&
      !receipts.some(({ state }) => state === 'submission-unknown')) ||
    (allKnown &&
      receipts.every(
        ({ state }) => state === 'pending' || state === 'in_ledger'
      ))
  ) {
    title = messages.submittedTitle;
    message = messages.submittedMessage;
    variant = styles.resultPending;
  } else if (
    result.status === 'submission-unknown' ||
    receipts.some(({ state }) => state === 'submission-unknown')
  ) {
    title = messages.submissionUnknownTitle;
    message = messages.submissionUnknownMessage;
    variant = styles.resultUnknown;
  } else if (cancelled) {
    title = messages.cancelledTitle;
    message = messages.cancelledMessage;
  } else if (deviceRejected) {
    title = messages.deviceRejectedTitle;
    message = messages.deviceRejectedMessage;
  }
  const failure = variant === styles.resultFailure;
  const stateMessages = {
    pending: messages.awaitingConfirmation,
    in_ledger: messages.confirmedStatus,
    expired: messages.expiredStatus,
    failed: messages.failedStatus,
    'submission-unknown': messages.submissionUnknownTitle,
  };

  return (
    <section
      className={`${styles.result} ${variant}`}
      role={failure ? 'alert' : 'status'}
      aria-live={failure ? 'assertive' : 'polite'}
    >
      <h2>{intl.formatMessage(title)}</h2>
      <p>
        {intl.formatMessage(
          message,
          failedItem === undefined ? undefined : { failedItem }
        )}
      </p>
      {transactionIds.length > 0 && (
        <div className={styles.resultIds}>
          <ol>
            {transactionIds.map((transactionId, index) => {
              const receipt = receipts.find(({ id }) => id === transactionId);
              const item =
                request.items.find(
                  ({ evidence }) =>
                    evidence.kind === 'exact-cbor' &&
                    evidence.review.transactionId === transactionId
                ) ||
                (request.items[index]?.evidence.kind === 'native-plan'
                  ? request.items[index]
                  : undefined);
              let fee: string | undefined;
              if (receipt?.amountIsKnown !== false && receipt?.fee) {
                fee = formattedWalletAmount(receipt.fee, true, true);
              } else if (item) {
                fee = ada(item.display.fee);
              }
              let walletChange: string | undefined;
              if (receipt?.amount && receipt.amountIsKnown !== false) {
                walletChange = formattedWalletAmount(
                  receipt.amount,
                  true,
                  true
                );
              } else if (item?.display.walletChange) {
                walletChange = ada(item.display.walletChange.coin);
              }
              let stateMessage = messages.awaitingConfirmation;
              if (result.status === 'signed')
                stateMessage = messages.signedTitle;
              if (result.status === 'submission-unknown') {
                stateMessage = messages.submissionUnknownTitle;
              }
              if (receipt) stateMessage = stateMessages[receipt.state];
              return (
                <li key={`${transactionId}:${index}`}>
                  <div className={styles.receiptState}>
                    <strong>{intl.formatMessage(stateMessage)}</strong>
                    {receipt?.state === 'in_ledger' &&
                      receipt.confirmations !== undefined && (
                        <span>
                          {intl.formatMessage(messages.confirmations, {
                            count: receipt.confirmations,
                          })}
                        </span>
                      )}
                  </div>
                  {receipt?.isSelfTransfer && (
                    <p>{intl.formatMessage(messages.withinWallet)}</p>
                  )}
                  {result.status !== 'signed' &&
                    (fee !== undefined || walletChange !== undefined) && (
                      <dl className={styles.receiptFacts}>
                        {receipt?.transferAmount && (
                          <>
                            <dt>
                              {intl.formatMessage(messages.transferAmount)}
                            </dt>
                            <dd>
                              {formattedWalletAmount(
                                receipt.transferAmount,
                                true,
                                true
                              )}
                            </dd>
                          </>
                        )}
                        {fee !== undefined && (
                          <>
                            <dt>{intl.formatMessage(messages.networkFee)}</dt>
                            <dd>{fee}</dd>
                          </>
                        )}
                        {walletChange !== undefined && (
                          <>
                            <dt>{intl.formatMessage(messages.walletChange)}</dt>
                            <dd>{walletChange}</dd>
                          </>
                        )}
                      </dl>
                    )}
                  <span>
                    {intl.formatMessage(messages.transactionId, {
                      value: index + 1,
                    })}
                  </span>
                  <code>{safeText(transactionId)}</code>
                  <div className={styles.receiptActions}>
                    <CopyToClipboard text={transactionId}>
                      <button type="button" className={styles.copy}>
                        {intl.formatMessage(messages.copyTransactionId)}
                      </button>
                    </CopyToClipboard>
                    {transactionIds.length > 1 &&
                      request.operation !== 'sign' &&
                      onViewTransaction && (
                        <button
                          type="button"
                          className={styles.copy}
                          onClick={() => onViewTransaction(transactionId)}
                        >
                          {intl.formatMessage(messages.viewTransaction)}
                        </button>
                      )}
                  </div>
                </li>
              );
            })}
          </ol>
        </div>
      )}
    </section>
  );
}

const acknowledgementMessages: Record<
  TransactionApprovalRequest['acknowledgements'][number],
  keyof typeof messages
> = {
  'flight-mainnet-funds': 'flightMainnetFunds',
  'undelegation-network-support': 'undelegationNetworkSupport',
  'undelegation-rewards': 'undelegationRewards',
};

export function TransactionApprovalDialog(props: Props) {
  const { request, intl } = props;
  const [passphrase, setPassphrase] = useState('');
  const [acknowledgements, setAcknowledgements] = useState<ReadonlySet<string>>(
    new Set()
  );
  const scrollRef = useRef<HTMLDivElement | null>(null);

  useEffect(() => {
    setPassphrase('');
    setAcknowledgements(new Set());
    const timeout = setTimeout(() => {
      if (props.result && scrollRef.current) scrollRef.current.scrollTop = 0;
      const heading = (props.result
        ? scrollRef.current?.querySelector(
            '[role="status"] h2, [role="alert"] h2'
          )
        : scrollRef.current?.parentElement?.querySelector(
            'h1'
          )) as HTMLElement | null;
      if (heading) {
        heading.tabIndex = -1;
        heading.focus();
      }
    });
    return () => clearTimeout(timeout);
  }, [request.requestId, props.result]);

  const approvable =
    request.items.length > 0 &&
    request.items.every(({ approvable: itemApprovable }) => itemApprovable);
  const acknowledged = request.acknowledgements.every((value) =>
    acknowledgements.has(value)
  );
  const software = request.authorization.kind === 'software';
  const total = request.items.length;
  const status = useMemo(() => {
    switch (props.phase) {
      case 'waiting-for-device':
        return intl.formatMessage(messages.waiting);
      case 'signing':
        return intl.formatMessage(messages.signing);
      case 'submitting':
        return intl.formatMessage(messages.submitting, {
          current: (props.activeItemIndex || 0) + 1,
          total,
        });
      default:
        return null;
    }
  }, [props.phase, props.activeItemIndex, total, intl]);
  let approveMessage = operationMessages[request.operation];
  if (request.authorization.kind === 'hardware') {
    approveMessage =
      request.operation === 'sign-and-submit'
        ? messages.deviceAndSend
        : messages.device;
  }
  const approveLabel = intl.formatMessage(approveMessage);
  const approve = () => {
    const value = passphrase;
    setPassphrase('');
    props.onApprove(software ? value : undefined);
  };
  const reject = () => {
    setPassphrase('');
    props.onReject();
  };
  const requester =
    request.requester.kind === 'dapp'
      ? request.requester.origin
      : `Daedalus · ${request.requester.action}`;
  let receiptIds: readonly string[] = [];
  if (props.result) {
    receiptIds =
      'transactionIds' in props.result
        ? props.result.transactionIds
        : (props.receipts || []).map(({ id }) => id);
  }

  return (
    <Dialog
      className={styles.component}
      title={intl.formatMessage(
        props.result ? messages.receiptTitle : messages.title
      )}
      fullSize
      wide
      closeOnOverlayClick={false}
      primaryButtonAutoFocus={false}
      scrollWrapperRef={scrollRef}
      footer={
        props.result ? undefined : (
          <div className={styles.footer}>
            {software && props.phase === 'ready' && (
              <label htmlFor="transaction-approval-password">
                {intl.formatMessage(messages.password)}
                <input
                  id="transaction-approval-password"
                  type="password"
                  value={passphrase}
                  onChange={(event) => setPassphrase(event.target.value)}
                  autoComplete="current-password"
                  disabled={props.deciding || !approvable}
                />
              </label>
            )}
            {props.errorCode && (
              <p className={styles.error} role="alert">
                {props.errorCode}
              </p>
            )}
            <p aria-live="polite">
              {status ||
                intl.formatMessage(
                  request.operation === 'sign'
                    ? messages.signingGuidance
                    : messages.submissionGuidance
                )}
            </p>
          </div>
        )
      }
      actions={
        props.result
          ? [
              ...(request.operation !== 'sign' &&
              receiptIds.length === 1 &&
              props.onViewTransaction
                ? [
                    {
                      label: intl.formatMessage(messages.viewTransaction),
                      onClick: () => props.onViewTransaction?.(receiptIds[0]),
                    },
                  ]
                : []),
              {
                className: 'confirmButton',
                label: intl.formatMessage(messages.done),
                onClick: props.onDismiss,
                disabled: !props.onDismiss,
                primary: true,
              },
            ]
          : [
              {
                label:
                  props.deciding && props.canCancel
                    ? intl.formatMessage(messages.cancel)
                    : intl.formatMessage(messages.reject),
                onClick:
                  props.deciding && props.canCancel ? props.onCancel : reject,
                disabled:
                  props.cancelling || (props.deciding && !props.canCancel),
              },
              {
                className: 'confirmButton',
                label: approveLabel,
                onClick: approve,
                disabled:
                  props.deciding ||
                  !approvable ||
                  !acknowledged ||
                  (software && !passphrase),
                primary: true,
              },
            ]
      }
    >
      <div className={styles.identity}>
        <bdi>{requester}</bdi>
        <span>
          {intl.formatMessage(messages.network, {
            value: request.networkName,
          })}
        </span>
        <strong>
          {intl.formatMessage(messages.wallet, {
            value: request.walletName,
          })}
        </strong>
      </div>
      {props.result ? (
        <>
          <ResultSummary
            result={props.result}
            request={request}
            receipts={props.receipts}
            onViewTransaction={props.onViewTransaction}
            intl={intl}
          />
          <details className={styles.reviewedDetails}>
            <summary>{intl.formatMessage(messages.reviewedDetails)}</summary>
            {request.items.map((reviewItem) => (
              <ItemReview
                key={reviewItem.index}
                item={reviewItem}
                props={props}
              />
            ))}
          </details>
        </>
      ) : (
        request.items.map((reviewItem) => (
          <ItemReview key={reviewItem.index} item={reviewItem} props={props} />
        ))
      )}
      {!props.result && request.acknowledgements.length > 0 && (
        <section className={styles.acknowledgements}>
          {request.acknowledgements.map((value) => (
            <label
              key={value}
              htmlFor={`transaction-approval-${request.requestId}-${value}`}
            >
              <input
                id={`transaction-approval-${request.requestId}-${value}`}
                type="checkbox"
                checked={acknowledgements.has(value)}
                onChange={(event) => {
                  const next = new Set(acknowledgements);
                  if (event.target.checked) next.add(value);
                  else next.delete(value);
                  setAcknowledgements(next);
                }}
                disabled={props.deciding}
              />
              <span>
                {intl.formatMessage(messages[acknowledgementMessages[value]])}
              </span>
            </label>
          ))}
        </section>
      )}
    </Dialog>
  );
}

export default injectIntl(TransactionApprovalDialog);
