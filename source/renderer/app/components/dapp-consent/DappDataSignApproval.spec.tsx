import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import StoryDecorator from '../../../../../storybook/stories/_support/StoryDecorator';
import translations from '../../i18n/locales/en-US.json';
import DappDataSignApproval from './DappDataSignApproval';

const request = (utf8Preview: string | null) => ({
  requestId: 'request',
  kind: 'data-sign' as const,
  origin: 'https://dapp.test/<script>',
  walletName: 'Wallet',
  networkName: 'Preview',
  scopes: ['data-signing'],
  extensions: [],
  review: {
    address: `60${'11'.repeat(28)}`,
    credentialKind: 'payment' as const,
    payload: Buffer.from('Hello', 'utf8').toString('hex'),
    utf8Preview,
  },
});

const renderApproval = (utf8Preview: string | null, onApprove = jest.fn()) =>
  render(
    <StoryDecorator>
      <IntlProvider locale="en-US" messages={translations}>
        <DappDataSignApproval
          request={request(utf8Preview)}
          deciding={false}
          onApprove={onApprove}
          onReject={jest.fn()}
        />
      </IntlProvider>
    </StoryDecorator>
  );

describe('DappDataSignApproval', () => {
  afterEach(cleanup);

  it('shows authoritative hex, escaped safe preview, and forwards only password', () => {
    const onApprove = jest.fn();
    const { container } = renderApproval('Hello <script>', onApprove);
    expect(screen.getByText(request(null).review.address)).toBeVisible();
    expect(screen.getByText(request(null).review.payload)).toBeVisible();
    expect(screen.getByText('Hello <script>')).toBeVisible();
    expect(container.querySelector('script')).toBeNull();

    const approve = screen.getByRole('button', { name: 'Sign data' });
    expect(approve).toBeDisabled();
    const password = screen.getByLabelText('Wallet spending password');
    fireEvent.change(password, { target: { value: 'secret' } });
    expect(approve).toBeEnabled();
    fireEvent.click(approve);
    expect(onApprove).toHaveBeenCalledWith('secret');
    expect(password).toHaveValue('');
  });

  it('explicitly omits unsafe UTF-8 preview', () => {
    renderApproval(null);
    expect(
      screen.getByText(
        'No safe UTF-8 preview is available. Review the exact hex.'
      )
    ).toBeVisible();
  });

  it('shows the normalized DRep credential identity', () => {
    const drep = {
      ...request('Governance'),
      scopes: ['governance-data-signing'],
      review: {
        ...request('Governance').review,
        address: '33'.repeat(28),
        credentialKind: 'drep' as const,
      },
    };
    render(
      <StoryDecorator>
        <IntlProvider locale="en-US" messages={translations}>
          <DappDataSignApproval
            request={drep}
            deciding={false}
            onApprove={jest.fn()}
            onReject={jest.fn()}
          />
        </IntlProvider>
      </StoryDecorator>
    );
    expect(screen.getByText('Credential: drep')).toBeVisible();
    expect(screen.getByText(drep.review.address)).toBeVisible();
  });
});
