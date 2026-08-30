import React, { useState } from 'react';
import { injectIntl } from 'react-intl';
import type { DappConsentPresentation } from '../../../../common/ipc/api';
import { CIP30_REVIEW_EFFECTS } from '../../../../common/cip30/review';
import type { Intl } from '../../types/i18nTypes';
import Dialog from '../widgets/Dialog';
import { CollapsibleSection } from '../widgets/collapsible-section/CollapsibleSection';
import { MonospaceTextBlock } from '../widgets/monospace-text-block/MonospaceTextBlock';
import { messages as transactionMessages } from './Cip30TransactionApproval.messages';
import { messages as dataSignMessages } from '../dapp-consent/DappDataSignApproval.messages';
import { messages } from './DappBatchReviewDialog.messages';
import styles from './DappBatchReviewDialog.scss';

type BatchPresentation = Extract<
  DappConsentPresentation,
  { kind: 'batch-sign' | 'batch-submit' }
>;

type Props = {
  intl: Intl;
  request: BatchPresentation;
  deciding: boolean;
  onApprove: (passphrase?: string) => void;
  onReject: () => void;
};

const outpoint = (value: Readonly<{ transactionId: string; index: number }>) =>
  `${value.transactionId}#${value.index}`;

const transactionApprovable = (
  item: BatchPresentation['review']['items'][number]
): boolean =>
  item.transaction.approvable &&
  item.transaction.commitmentsVerified &&
  item.transaction.refusalReasons.length === 0 &&
  item.transaction.effects.every(({ kind }) =>
    (CIP30_REVIEW_EFFECTS as readonly string[]).includes(kind)
  ) &&
  !item.transaction.effects.some(
    ({ kind }) => kind === 'maximum-collateral-loss-unresolved'
  );

export function DappBatchReviewDialog({
  intl,
  request,
  deciding,
  onApprove,
  onReject,
}: Props) {
  const signing = request.kind === 'batch-sign';
  const [passphrase, setPassphrase] = useState('');
  const itemApprovals = request.review.items.map(transactionApprovable);
  const refusalIndex = itemApprovals.findIndex((value) => !value);
  const approvable =
    request.review.approvable &&
    refusalIndex === -1 &&
    request.review.items.length > 0;
  const approve = () => {
    const value = passphrase;
    setPassphrase('');
    onApprove(signing ? value : undefined);
  };
  const reject = () => {
    setPassphrase('');
    onReject();
  };

  return (
    <Dialog
      className={styles.component}
      title={intl.formatMessage(
        signing ? messages.signTitle : messages.submitTitle
      )}
      actions={[
        {
          label: intl.formatMessage(transactionMessages.reject),
          onClick: reject,
          disabled: deciding,
        },
        {
          label: intl.formatMessage(signing ? messages.sign : messages.submit),
          onClick: approve,
          disabled: deciding || !approvable || (signing && !passphrase),
          primary: true,
        },
      ]}
      closeOnOverlayClick={false}
      fullSize
      primaryButtonAutoFocus={!signing && approvable}
    >
      <section aria-labelledby="cip103-review-identity">
        <h2 id="cip103-review-identity">
          {intl.formatMessage(transactionMessages.identity)}
        </h2>
        <p>
          {intl.formatMessage(transactionMessages.origin, {
            value: request.origin,
          })}
        </p>
        <p>
          {intl.formatMessage(transactionMessages.wallet, {
            value: request.walletName,
          })}
        </p>
        <p>
          {intl.formatMessage(transactionMessages.network, {
            value: request.networkName,
          })}
        </p>
      </section>

      {refusalIndex !== -1 && (
        <div className={styles.refusal} role="alert">
          {intl.formatMessage(messages.refusal, {
            current: refusalIndex + 1,
          })}
        </div>
      )}

      <p className={styles.notice} role="note">
        {intl.formatMessage(messages.perItemNotice)}
      </p>

      <ol className={styles.items}>
        {request.review.items.map((item, index) => {
          const headingId = `cip103-review-item-${index}`;
          const supported = itemApprovals[index];
          return (
            <li key={item.index}>
              <article className={styles.item} aria-labelledby={headingId}>
                <header>
                  <h2 id={headingId}>
                    {intl.formatMessage(messages.itemPosition, {
                      current: index + 1,
                      total: request.review.items.length,
                    })}
                  </h2>
                  <span className={supported ? styles.ready : styles.blocked}>
                    {intl.formatMessage(
                      supported ? messages.itemReady : messages.itemBlocked
                    )}
                  </span>
                </header>

                {!supported && (
                  <div className={styles.refusal} role="alert">
                    {item.transaction.refusalReasons.map((reason) => (
                      <MonospaceTextBlock key={reason}>
                        {reason}
                      </MonospaceTextBlock>
                    ))}
                  </div>
                )}

                <section aria-labelledby={`${headingId}-dependencies`}>
                  <h3 id={`${headingId}-dependencies`}>
                    {intl.formatMessage(messages.dependencies)}
                  </h3>
                  {item.dependencies.length === 0 ? (
                    <p>{intl.formatMessage(messages.noDependencies)}</p>
                  ) : (
                    <ul>
                      {item.dependencies.map((dependency) => (
                        <li
                          key={`${dependency.inputRole}:${outpoint(
                            dependency.outpoint
                          )}`}
                        >
                          <p>
                            {intl.formatMessage(
                              dependency.source === 'current-batch'
                                ? messages.currentDependency
                                : messages.pendingDependency,
                              {
                                role: dependency.inputRole,
                                current:
                                  (dependency.sourceTransactionIndex ?? 0) + 1,
                              }
                            )}
                          </p>
                          <MonospaceTextBlock>
                            {outpoint(dependency.outpoint)}
                          </MonospaceTextBlock>
                        </li>
                      ))}
                    </ul>
                  )}
                </section>

                {item.conflicts.length > 0 && (
                  <section
                    className={styles.conflicts}
                    aria-labelledby={`${headingId}-conflicts`}
                  >
                    <h3 id={`${headingId}-conflicts`}>
                      {intl.formatMessage(messages.conflicts)}
                    </h3>
                    {item.conflicts.map((conflict) => (
                      <div
                        key={`${conflict.inputRole}:${outpoint(
                          conflict.outpoint
                        )}`}
                        role="status"
                      >
                        <p>
                          {intl.formatMessage(messages.conflict, {
                            role: conflict.inputRole,
                            current: conflict.earlierTransactionIndex + 1,
                          })}
                        </p>
                        <MonospaceTextBlock>
                          {outpoint(conflict.outpoint)}
                        </MonospaceTextBlock>
                      </div>
                    ))}
                  </section>
                )}

                <section aria-labelledby={`${headingId}-effects`}>
                  <h3 id={`${headingId}-effects`}>
                    {intl.formatMessage(messages.effects)}
                  </h3>
                  {item.transaction.effects.map((effect) => (
                    <CollapsibleSection
                      key={effect.index}
                      header={`${effect.index + 1}. ${effect.kind}`}
                    >
                      <MonospaceTextBlock>{effect.value}</MonospaceTextBlock>
                    </CollapsibleSection>
                  ))}
                </section>

                {signing && (
                  <div className={styles.warning} role="note">
                    <p>
                      {intl.formatMessage(
                        transactionMessages.collateralWarning
                      )}
                    </p>
                    <h3>
                      {intl.formatMessage(
                        transactionMessages.maximumCollateral
                      )}
                    </h3>
                    <MonospaceTextBlock>
                      {item.transaction.maximumCollateralLoss ||
                        intl.formatMessage(transactionMessages.noCollateral)}
                    </MonospaceTextBlock>
                  </div>
                )}

                <CollapsibleSection
                  header={intl.formatMessage(transactionMessages.bodyHash)}
                >
                  <MonospaceTextBlock>
                    {item.transaction.transactionId}
                  </MonospaceTextBlock>
                </CollapsibleSection>
                {!signing && (
                  <CollapsibleSection
                    header={intl.formatMessage(transactionMessages.outerDigest)}
                  >
                    <MonospaceTextBlock>
                      {item.transaction.fullCborDigest}
                    </MonospaceTextBlock>
                  </CollapsibleSection>
                )}
              </article>
            </li>
          );
        })}
      </ol>

      <section aria-labelledby="cip103-review-execution">
        <h2 id="cip103-review-execution">
          {intl.formatMessage(
            signing ? messages.signTitle : messages.submitTitle
          )}
        </h2>
        {signing ? (
          <>
            <p>{intl.formatMessage(messages.softwareSigning)}</p>
            <p>{intl.formatMessage(messages.deviceSigning)}</p>
            <label
              className={styles.passwordLabel}
              htmlFor="cip103-sign-password"
            >
              {intl.formatMessage(dataSignMessages.password)}
            </label>
            <input
              id="cip103-sign-password"
              className={styles.password}
              type="password"
              value={passphrase}
              onChange={(event) => setPassphrase(event.target.value)}
              autoComplete="current-password"
              disabled={deciding || !approvable}
            />
          </>
        ) : (
          <>
            <p>{intl.formatMessage(messages.submission)}</p>
            <p>{intl.formatMessage(messages.recovery)}</p>
          </>
        )}
      </section>
    </Dialog>
  );
}

export default injectIntl(DappBatchReviewDialog);
