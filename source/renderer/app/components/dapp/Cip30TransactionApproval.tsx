import React, { useState } from 'react';
import { injectIntl } from 'react-intl';
import type { DappConsentPresentation } from '../../../../common/ipc/api';
import { CIP30_REVIEW_EFFECTS } from '../../../../common/cip30/review';
import type { Intl } from '../../types/i18nTypes';
import Dialog from '../widgets/Dialog';
import { CollapsibleSection } from '../widgets/collapsible-section/CollapsibleSection';
import { MonospaceTextBlock } from '../widgets/monospace-text-block/MonospaceTextBlock';
import { messages } from './Cip30TransactionApproval.messages';
import { messages as dataSignMessages } from '../dapp-consent/DappDataSignApproval.messages';
import collateralMessages from './collateral/CollateralPanel.messages';
import styles from './Cip30TransactionApproval.scss';

type TransactionPresentation = Extract<
  DappConsentPresentation,
  { kind: 'transaction-sign' | 'transaction-submit' }
>;

type Props = {
  intl: Intl;
  request: TransactionPresentation;
  deciding: boolean;
  onApprove: (passphrase?: string) => void;
  onReject: () => void;
};

export function Cip30TransactionApproval({
  intl,
  request,
  deciding,
  onApprove,
  onReject,
}: Props) {
  const { review } = request;
  const supported = review.effects.every(({ kind }) =>
    (CIP30_REVIEW_EFFECTS as readonly string[]).includes(kind)
  );
  const approvable =
    review.approvable &&
    review.commitmentsVerified &&
    review.refusalReasons.length === 0 &&
    supported &&
    !review.effects.some(
      ({ kind }) => kind === 'maximum-collateral-loss-unresolved'
    );
  const signing = request.kind === 'transaction-sign';
  const [passphrase, setPassphrase] = useState('');
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
          label: intl.formatMessage(messages.reject),
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
      primaryButtonAutoFocus={approvable}
    >
      <section aria-labelledby="cip30-review-identity">
        <h2 id="cip30-review-identity">
          {intl.formatMessage(messages.identity)}
        </h2>
        <p>{intl.formatMessage(messages.origin, { value: request.origin })}</p>
        <p>
          {intl.formatMessage(messages.wallet, { value: request.walletName })}
        </p>
        <p>
          {intl.formatMessage(messages.network, { value: request.networkName })}
        </p>
      </section>

      {!approvable && (
        <div className={styles.refusal} role="alert">
          <strong>{intl.formatMessage(messages.refusal)}</strong>
          {review.refusalReasons.map((reason) => (
            <MonospaceTextBlock key={reason}>{reason}</MonospaceTextBlock>
          ))}
        </div>
      )}

      {review.effects.some(
        ({ kind }) => kind === 'preferred-collateral-spend'
      ) && (
        <p className={styles.refusal} role="alert">
          {intl.formatMessage(collateralMessages.willBeSpent)}
        </p>
      )}

      <section aria-labelledby="cip30-review-effects">
        <h2 id="cip30-review-effects">
          {intl.formatMessage(messages.effects)}
        </h2>
        {review.effects.map((effect) => (
          <CollapsibleSection
            key={effect.index}
            header={`${effect.index + 1}. ${effect.kind}`}
          >
            <MonospaceTextBlock>{effect.value}</MonospaceTextBlock>
          </CollapsibleSection>
        ))}
      </section>

      <section aria-labelledby="cip30-review-verification">
        <h2 id="cip30-review-verification">
          {intl.formatMessage(messages.verification)}
        </h2>
        {review.commitmentsVerified && (
          <p className={styles.verified}>
            {intl.formatMessage(messages.commitmentsVerified)}
          </p>
        )}
        <CollapsibleSection header={intl.formatMessage(messages.bodyHash)}>
          <MonospaceTextBlock>{review.transactionId}</MonospaceTextBlock>
        </CollapsibleSection>
        <CollapsibleSection header={intl.formatMessage(messages.bodyCbor)}>
          <MonospaceTextBlock>{review.bodyCbor}</MonospaceTextBlock>
        </CollapsibleSection>
        {review.auxiliaryDataHash && (
          <CollapsibleSection
            header={intl.formatMessage(messages.auxiliaryHash)}
          >
            <MonospaceTextBlock>{review.auxiliaryDataHash}</MonospaceTextBlock>
          </CollapsibleSection>
        )}
        {review.scriptDataHash && (
          <CollapsibleSection
            header={intl.formatMessage(messages.scriptDataHash)}
          >
            <MonospaceTextBlock>{review.scriptDataHash}</MonospaceTextBlock>
          </CollapsibleSection>
        )}
        {review.existingVkeyWitnesses.length > 0 && (
          <CollapsibleSection
            header={intl.formatMessage(messages.vkeyWitnesses)}
          >
            <MonospaceTextBlock>
              {review.existingVkeyWitnesses.join('\n')}
            </MonospaceTextBlock>
          </CollapsibleSection>
        )}
        {review.existingBootstrapWitnesses.length > 0 && (
          <CollapsibleSection
            header={intl.formatMessage(messages.bootstrapWitnesses)}
          >
            <MonospaceTextBlock>
              {review.existingBootstrapWitnesses.join('\n')}
            </MonospaceTextBlock>
          </CollapsibleSection>
        )}
        {signing ? (
          <div className={styles.warning} role="note">
            <p>{intl.formatMessage(messages.collateralWarning)}</p>
            <h3>{intl.formatMessage(messages.maximumCollateral)}</h3>
            <MonospaceTextBlock>
              {review.maximumCollateralLoss ||
                intl.formatMessage(messages.noCollateral)}
            </MonospaceTextBlock>
          </div>
        ) : (
          <>
            <p>
              {intl.formatMessage(messages.isValid, {
                value: String(review.isValid),
              })}
            </p>
            <CollapsibleSection
              header={intl.formatMessage(messages.outerDigest)}
            >
              <MonospaceTextBlock>{review.fullCborDigest}</MonospaceTextBlock>
            </CollapsibleSection>
            <CollapsibleSection header={intl.formatMessage(messages.witnesses)}>
              <MonospaceTextBlock>{review.witnessSetCbor}</MonospaceTextBlock>
            </CollapsibleSection>
            <CollapsibleSection header={intl.formatMessage(messages.auxiliary)}>
              <MonospaceTextBlock>
                {review.auxiliaryDataCbor}
              </MonospaceTextBlock>
            </CollapsibleSection>
            <CollapsibleSection header={intl.formatMessage(messages.outerCbor)}>
              <MonospaceTextBlock>{review.fullCbor}</MonospaceTextBlock>
            </CollapsibleSection>
          </>
        )}
        {signing && (
          <>
            <label
              className={styles.passwordLabel}
              htmlFor="cip30-sign-password"
            >
              {intl.formatMessage(dataSignMessages.password)}
            </label>
            <input
              id="cip30-sign-password"
              className={styles.password}
              type="password"
              value={passphrase}
              onChange={(event) => setPassphrase(event.target.value)}
              autoComplete="current-password"
              disabled={deciding || !approvable}
            />
          </>
        )}
      </section>
    </Dialog>
  );
}

export default injectIntl(Cip30TransactionApproval);
