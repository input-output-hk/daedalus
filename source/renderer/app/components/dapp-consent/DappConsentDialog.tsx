import React from 'react';
import { injectIntl } from 'react-intl';
import type { DappConsentPresentation } from '../../../../common/ipc/api';
import type { Intl } from '../../types/i18nTypes';
import Dialog from '../widgets/Dialog';
import { messages } from './DappConsentDialog.messages';

type Props = {
  intl: Intl;
  request: DappConsentPresentation;
  deciding: boolean;
  onApprove: () => void;
  onReject: () => void;
};

function DappConsentDialog({
  intl,
  request,
  deciding,
  onApprove,
  onReject,
}: Props) {
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
          onClick: onReject,
          disabled: deciding,
        },
        {
          label: intl.formatMessage(messages.approve),
          onClick: onApprove,
          disabled: deciding,
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
      {request.kind === 'key-disclosure' && (
        <p>{intl.formatMessage(messages.disclosureWarning)}</p>
      )}
    </Dialog>
  );
}

export default injectIntl(DappConsentDialog);
