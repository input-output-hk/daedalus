import BigNumber from 'bignumber.js';
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

const unsafeText = /[\u0000-\u001f\u007f\u202a-\u202e\u2066-\u2069]/gu;
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
const badge = (entry: TransactionReviewEntry, intl: Intl) =>
  entry.ownership === 'wallet'
    ? intl.formatMessage(messages.thisWallet)
    : entry.control === 'script'
    ? intl.formatMessage(messages.script)
    : entry.ownership === 'other'
    ? intl.formatMessage(messages.other)
    : intl.formatMessage(messages.ownershipUnknown);

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

type Props = TransactionApprovalDialogProps & { intl: Intl };

const AssetRow = ({
  asset,
  props,
  sign,
}: {
  asset: TransactionReviewAsset;
  props: Props;
  sign?: '+' | '−';
}) => {
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
  return (
    <div className={styles.assetRow}>
      <div>
        <span className={styles.amount}>
          {sign || ''}
          {amount}
        </span>{' '}
        <bdi>{name}</bdi>
      </div>
      <code>{asset.fingerprint}</code>
      <details>
        <summary>{asset.fingerprint}</summary>
        <AssetContent asset={details} displayMode="review" />
        <p>
          <code>{asset.quantity}</code> base units
        </p>
      </details>
    </div>
  );
};

const ValueRows = ({
  value,
  props,
}: {
  value: TransactionReviewValue | null;
  props: Props;
}) =>
  value ? (
    <>
      <div className={styles.amount}>{ada(value.coin)}</div>
      {value.assets.map((asset) => (
        <AssetRow
          key={`${asset.policyId}:${asset.assetName}`}
          asset={asset}
          props={props}
        />
      ))}
    </>
  ) : (
    <p>{props.intl.formatMessage(messages.valueUnavailable)}</p>
  );

const ImpactCard = ({
  direction,
  value,
  props,
}: {
  direction: 'leaving' | 'coming';
  value: TransactionReviewValue | null;
  props: Props;
}) => {
  if (!value)
    return (
      <div className={styles.impactCard}>
        <h3>{props.intl.formatMessage(messages[direction])}</h3>
        <p>{props.intl.formatMessage(messages.unavailable)}</p>
      </div>
    );
  const leaving = direction === 'leaving';
  const coinMatches = leaving
    ? new BigNumber(value.coin).isNegative()
    : new BigNumber(value.coin).isPositive();
  const assets = value.assets.filter(({ quantity }) =>
    leaving
      ? new BigNumber(quantity).isNegative()
      : new BigNumber(quantity).isPositive()
  );
  return (
    <div className={styles.impactCard}>
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
};

const EntryCard = ({
  entry,
  props,
}: {
  entry: TransactionReviewEntry;
  props: Props;
}) => {
  const address = entry.address;
  const point = entry.outpoint
    ? `${entry.outpoint.transactionId}#${entry.outpoint.index}`
    : null;
  const output = entry.role === 'output' || entry.role === 'collateral-return';
  return (
    <article className={styles.entryCard}>
      <header>
        <h3>
          {props.intl.formatMessage(output ? messages.output : messages.input, {
            value: entry.position + 1,
          })}
        </h3>
        <span className={styles.badge}>{badge(entry, props.intl)}</span>
      </header>
      <ValueRows value={entry.value} props={props} />
      {address && (
        <details>
          <summary>{preview(address)}</summary>
          <code className={styles.fullText}>{address}</code>
          <CopyToClipboard text={address}>
            <button type="button" className={styles.copy}>
              {props.intl.formatMessage(messages.copyAddress)}
            </button>
          </CopyToClipboard>
        </details>
      )}
      {point && (
        <div className={styles.identity}>
          <span>{props.intl.formatMessage(messages.outpoint)}</span>
          <code>{preview(point)}</code>
          <details>
            <summary>{props.intl.formatMessage(messages.fullAddress)}</summary>
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
};

const EntrySection = ({
  entries,
  title,
  props,
}: {
  entries: readonly TransactionReviewEntry[];
  title: string;
  props: Props;
}) =>
  entries.length ? (
    <section>
      <h2 tabIndex={-1}>{title}</h2>
      <div className={styles.entryGrid}>
        {entries.map((entry) => (
          <EntryCard
            key={`${entry.role}:${entry.position}`}
            entry={entry}
            props={props}
          />
        ))}
      </div>
    </section>
  ) : null;

const OtherActions = ({
  item,
  props,
}: {
  item: TransactionApprovalItem;
  props: Props;
}) => {
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
};

const ItemReview = ({
  item,
  props,
}: {
  item: TransactionApprovalItem;
  props: Props;
}) => {
  const { display } = item;
  const inputs = display.entries.filter(({ role }) => role === 'input');
  const outputs = display.entries.filter(({ role }) => role === 'output');
  const references = display.entries.filter(
    ({ role }) => role === 'reference-input'
  );
  const collateral = display.entries.filter(
    ({ role }) => role === 'collateral-input' || role === 'collateral-return'
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
        <div>
          <span>{props.intl.formatMessage(messages.fee)}</span>
          <strong>{ada(display.fee)}</strong>
        </div>
        {display.maximumCollateralLoss && (
          <div>
            <span>{props.intl.formatMessage(messages.collateral)}</span>
            <strong>
              {props.intl.formatMessage(messages.upTo, {
                value: ada(display.maximumCollateralLoss.coin),
              })}
            </strong>
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
      <div className={styles.columns}>
        <EntrySection
          entries={inputs}
          title={props.intl.formatMessage(messages.inputs)}
          props={props}
        />
        <EntrySection
          entries={outputs}
          title={props.intl.formatMessage(messages.outputs)}
          props={props}
        />
      </div>
      <EntrySection
        entries={references}
        title={props.intl.formatMessage(messages.referenceInputs)}
        props={props}
      />
      <EntrySection
        entries={collateral}
        title={props.intl.formatMessage(messages.collateralEntries)}
        props={props}
      />
      <OtherActions item={item} props={props} />
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
};

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
      const heading = scrollRef.current?.parentElement?.querySelector(
        'h1'
      ) as HTMLElement | null;
      if (heading) {
        heading.tabIndex = -1;
        heading.focus();
      }
    });
    return () => clearTimeout(timeout);
  }, [request.requestId]);

  const approvable =
    request.items.length > 0 &&
    request.items.every(({ approvable: itemApprovable }) => itemApprovable);
  const acknowledged = request.acknowledgements.every((value) =>
    acknowledgements.has(value)
  );
  const software = request.authorization.kind === 'software';
  const total = request.items.length;
  const status = useMemo(
    () =>
      props.phase === 'waiting-for-device'
        ? intl.formatMessage(messages.waiting)
        : props.phase === 'signing'
        ? intl.formatMessage(messages.signing)
        : props.phase === 'submitting'
        ? intl.formatMessage(messages.submitting, {
            current: (props.activeItemIndex || 0) + 1,
            total,
          })
        : null,
    [props.phase, props.activeItemIndex, total, intl]
  );
  const approveLabel =
    request.authorization.kind === 'hardware'
      ? intl.formatMessage(
          request.operation === 'sign-and-submit'
            ? messages.deviceAndSend
            : messages.device
        )
      : intl.formatMessage(
          request.operation === 'submit'
            ? messages.submit
            : request.operation === 'sign-and-submit'
            ? messages.signAndSend
            : messages.sign
        );
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

  return (
    <Dialog
      className={styles.component}
      title={intl.formatMessage(messages.title)}
      fullSize
      closeOnOverlayClick={false}
      primaryButtonAutoFocus={false}
      scrollWrapperRef={scrollRef}
      footer={
        <div className={styles.footer}>
          {software && props.phase === 'ready' && (
            <label>
              {intl.formatMessage(messages.password)}
              <input
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
          <p>
            {status ||
              intl.formatMessage(
                request.operation === 'sign'
                  ? messages.signingGuidance
                  : messages.submissionGuidance
              )}
          </p>
        </div>
      }
      actions={[
        {
          label:
            props.deciding && props.canCancel
              ? 'Cancel'
              : intl.formatMessage(messages.reject),
          onClick: props.deciding && props.canCancel ? props.onCancel : reject,
          disabled: props.cancelling || (props.deciding && !props.canCancel),
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
      ]}
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
      {request.items.map((reviewItem) => (
        <ItemReview key={reviewItem.index} item={reviewItem} props={props} />
      ))}
      {request.acknowledgements.length > 0 && (
        <section className={styles.acknowledgements}>
          {request.acknowledgements.map((value) => (
            <label key={value}>
              <input
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
