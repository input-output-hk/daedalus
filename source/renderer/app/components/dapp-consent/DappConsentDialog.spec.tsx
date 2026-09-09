import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import StoryDecorator from '../../../../../storybook/stories/_support/StoryDecorator';
import translations from '../../i18n/locales/en-US.json';
import DappConsentDialog from './DappConsentDialog';

const request = {
  requestId: 'request',
  walletId: 'aa'.repeat(20),
  kind: 'key-disclosure' as const,
  origin: 'https://evil.test/<script>',
  walletName: 'Wallet',
  networkName: 'Preview',
  scopes: ['governance-key-disclosure'],
  extensions: [95],
};

describe('DappConsentDialog', () => {
  afterEach(cleanup);

  it('renders trusted consent details and accessible decisions', () => {
    const onApprove = jest.fn();
    const onReject = jest.fn();
    const { container } = render(
      <StoryDecorator>
        <IntlProvider locale="en-US" messages={translations}>
          <DappConsentDialog
            request={request}
            deciding={false}
            onApprove={onApprove}
            onReject={onReject}
          />
        </IntlProvider>
      </StoryDecorator>
    );

    expect(screen.getByText(`Origin: ${request.origin}`)).toBeVisible();
    expect(screen.getByText('Extensions: CIP-95')).toBeVisible();
    expect(
      screen.getByText(
        'Stake and DRep public keys can correlate this wallet with governance activity. This permission is separate from the connection and can be revoked independently.'
      )
    ).toBeVisible();
    expect(container.querySelector('script')).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: 'Approve' }));
    fireEvent.click(screen.getByRole('button', { name: 'Reject' }));
    expect(onApprove).toHaveBeenCalledTimes(1);
    expect(onReject).toHaveBeenCalledTimes(1);
  });

  it('requires a password for irreversible account-key disclosure', () => {
    const onApprove = jest.fn();
    render(
      <StoryDecorator>
        <IntlProvider locale="en-US" messages={translations}>
          <DappConsentDialog
            request={{
              ...request,
              scopes: ['account-public-key-disclosure'],
              extensions: [104],
              requiresPassphrase: true,
            }}
            deciding={false}
            onApprove={onApprove}
            onReject={jest.fn()}
          />
        </IntlProvider>
      </StoryDecorator>
    );

    const approve = screen.getByRole('button', { name: 'Approve' });
    expect(approve).toBeDisabled();
    expect(
      screen.getByText(/complete address history and future derivation paths/)
    ).toBeVisible();
    fireEvent.change(screen.getByLabelText('Spending password'), {
      target: { value: 'secret' },
    });
    fireEvent.click(approve);
    expect(onApprove).toHaveBeenCalledWith('secret');
  });
});
