import React, { useState } from 'react';
import { injectIntl } from 'react-intl';
import type { DappConsentPresentation } from '../../../../common/ipc/api';
import type { Intl } from '../../types/i18nTypes';
import Dialog from '../widgets/Dialog';
import { MonospaceTextBlock } from '../widgets/monospace-text-block/MonospaceTextBlock';
import { messages } from './DappDataSignApproval.messages';
import styles from './DappDataSignApproval.scss';

type Props = {
  intl: Intl;
  request: Extract<DappConsentPresentation, { kind: 'data-sign' }>;
  deciding: boolean;
  onApprove: (passphrase?: string) => void;
  onReject: () => void;
};

export function DappDataSignApproval({
  intl,
  request,
  deciding,
  onApprove,
  onReject,
}: Props) {
  const [passphrase, setPassphrase] = useState('');
  const approve = () => {
    const value = passphrase;
    setPassphrase('');
    onApprove(value);
  };
  const reject = () => {
    setPassphrase('');
    onReject();
  };

  return (
    <Dialog
      className={styles.component}
      title={intl.formatMessage(messages.title)}
      actions={[
        {
          label: intl.formatMessage(messages.reject),
          onClick: reject,
          disabled: deciding,
        },
        {
          label: intl.formatMessage(messages.approve),
          onClick: approve,
          disabled: deciding || passphrase.length === 0,
          primary: true,
        },
      ]}
      closeOnOverlayClick={false}
      fullSize
    >
      <p>{intl.formatMessage(messages.origin, { value: request.origin })}</p>
      <p>
        {intl.formatMessage(messages.wallet, { value: request.walletName })}
      </p>
      <p>
        {intl.formatMessage(messages.network, { value: request.networkName })}
      </p>
      <p>
        {intl.formatMessage(messages.credential, {
          value: request.review.credentialKind,
        })}
      </p>
      <h2>{intl.formatMessage(messages.address)}</h2>
      <MonospaceTextBlock>{request.review.address}</MonospaceTextBlock>
      <h2>{intl.formatMessage(messages.payload)}</h2>
      <MonospaceTextBlock>{request.review.payload}</MonospaceTextBlock>
      <h2>{intl.formatMessage(messages.preview)}</h2>
      <pre className={styles.preview}>
        {request.review.utf8Preview ?? intl.formatMessage(messages.noPreview)}
      </pre>
      <label
        className={styles.passwordLabel}
        htmlFor="cip30-data-sign-password"
      >
        {intl.formatMessage(messages.password)}
      </label>
      <input
        id="cip30-data-sign-password"
        className={styles.password}
        type="password"
        value={passphrase}
        onChange={(event) => setPassphrase(event.target.value)}
        autoComplete="current-password"
        disabled={deciding}
      />
    </Dialog>
  );
}

export default injectIntl(DappDataSignApproval);
