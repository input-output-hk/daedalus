import React, { useState } from 'react';
import { injectIntl } from 'react-intl';
import type { DappConsentPresentation } from '../../../../common/ipc/api';
import type { Intl } from '../../types/i18nTypes';
import Dialog from '../widgets/Dialog';
import { messages } from './DappConsentDialog.messages';

type Props = {
  intl: Intl;
  request: DappConsentPresentation;
  deciding: boolean;
  onApprove: (passphrase?: string) => void;
  onReject: () => void;
};

function DappConsentDialog({
  intl,
  request,
  deciding,
  onApprove,
  onReject,
}: Props) {
  const [passphrase, setPassphrase] = useState('');
  const requiresPassphrase =
    request.kind === 'key-disclosure' && request.requiresPassphrase === true;
  const approve = () => {
    const value = passphrase;
    setPassphrase('');
    onApprove(requiresPassphrase ? value : undefined);
  };
  const reject = () => {
    setPassphrase('');
    onReject();
  };
  return (
    <Dialog
      title={intl.formatMessage(
        request.kind === 'key-disclosure'
          ? messages.disclosureTitle
          : messages.connectionTitle
      )}
      actions={[
        {
          label: intl.formatMessage(messages.reject),
          onClick: reject,
          disabled: deciding,
        },
        {
          label: intl.formatMessage(messages.approve),
          onClick: approve,
          disabled: deciding || (requiresPassphrase && passphrase.length === 0),
          primary: true,
        },
      ]}
      primaryButtonAutoFocus
    >
      <p>{intl.formatMessage(messages.origin, { origin: request.origin })}</p>
      <p>
        {intl.formatMessage(messages.wallet, { wallet: request.walletName })}
      </p>
      <p>
        {intl.formatMessage(messages.network, {
          network: request.networkName,
        })}
      </p>
      <p>
        {intl.formatMessage(messages.scopes, {
          scopes: request.scopes.join(', '),
        })}
      </p>
      {request.extensions.length > 0 && (
        <p>
          {intl.formatMessage(messages.extensions, {
            extensions: request.extensions
              .map((cip) => `CIP-${cip}`)
              .join(', '),
          })}
        </p>
      )}
      {request.kind === 'key-disclosure' &&
        request.scopes.includes('governance-key-disclosure') && (
          <p>{intl.formatMessage(messages.governanceDisclosureWarning)}</p>
        )}
      {request.kind === 'key-disclosure' &&
        request.scopes.includes('account-public-key-disclosure') && (
          <p>{intl.formatMessage(messages.accountDisclosureWarning)}</p>
        )}
      {requiresPassphrase && (
        <>
          <label htmlFor="cip104-account-public-key-password">
            {intl.formatMessage(messages.password)}
          </label>
          <input
            id="cip104-account-public-key-password"
            type="password"
            value={passphrase}
            onChange={(event) => setPassphrase(event.target.value)}
            autoComplete="current-password"
            disabled={deciding}
          />
        </>
      )}
    </Dialog>
  );
}

export default injectIntl(DappConsentDialog);
