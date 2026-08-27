import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import StoryDecorator from '../../../../../storybook/stories/_support/StoryDecorator';
import translations from '../../i18n/locales/en-US.json';
import DappConsentDialog from './DappConsentDialog';

const request = {
  requestId: 'request',
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
});
