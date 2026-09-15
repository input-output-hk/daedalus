import '@testing-library/jest-dom';

import React from 'react';
import BigNumber from 'bignumber.js';
import { cleanup, fireEvent, screen } from '@testing-library/react';

import createTestBed from 'tests/_utils/TestBed';

import WalletTokens from './WalletTokens';

jest.mock('../wallet-tokens-list/WalletTokensList', () => ({
  __esModule: true,
  default: () => <div data-testid="tokens-list" />,
}));

jest.mock('../wallet-tokens-search/WalletTokensSearch', () => ({
  __esModule: true,
  default: () => <div data-testid="tokens-search" />,
}));

const token = (uniqueId: string) => ({
  policyId: uniqueId.slice(0, 56),
  assetName: uniqueId.slice(56),
  uniqueId,
  fingerprint: 'asset1cvmyrfrc7lpsnjhhz9l4rzqmc6nlp4kw2xkvpa',
  quantity: new BigNumber('900000000'),
  decimals: 6,
  recommendedDecimals: 6,
  recommendedDecimalsVerified: true,
  metadata: { name: 'Test Coin', ticker: 'TEST', description: '' },
});

const held = [token(`${'a'.repeat(56)}42544544`)];

const renderTokens = (props: Record<string, any> = {}) =>
  createTestBed(
    <WalletTokens
      assets={held}
      currentLocale="en-US"
      onAssetSettings={() => {}}
      onCopyAssetParam={() => {}}
      onExternalLinkClick={() => {}}
      onOpenAssetSend={() => {}}
      onToggleFavorite={() => {}}
      tokenFavorites={{}}
      wallet={{ isRestoring: false } as any}
      {...props}
    />
  );

describe('WalletTokens', () => {
  afterEach(() => cleanup());

  describe('the one-time decimal places notice', () => {
    it('is shown to a profile that holds tokens and has not been told', () => {
      renderTokens({ isDecimalPlacesNoticeAcknowledged: false });
      const notice = screen.getByTestId('decimalPlacesNotice');
      // What changed about entering an amount, first and by example.
      expect(notice).toHaveTextContent('1.5 and not 1500000');
      // Displayed balances, second.
      expect(notice).toHaveTextContent('Balances for those tokens are shown');
      // And that a setting of the user's own still wins.
      expect(notice).toHaveTextContent('still overrides both');
    });

    it('is not shown to a profile holding no tokens', () => {
      renderTokens({ assets: [], isDecimalPlacesNoticeAcknowledged: false });
      expect(
        screen.queryByTestId('decimalPlacesNotice')
      ).not.toBeInTheDocument();
    });

    it('is not shown to a profile that has already been told', () => {
      renderTokens({ isDecimalPlacesNoticeAcknowledged: true });
      expect(
        screen.queryByTestId('decimalPlacesNotice')
      ).not.toBeInTheDocument();
    });

    it('is not shown when nothing says whether the profile has been told', () => {
      // The default is acknowledged, so a surface that has not been wired to the
      // profile store shows nothing rather than showing it on every render.
      renderTokens();
      expect(
        screen.queryByTestId('decimalPlacesNotice')
      ).not.toBeInTheDocument();
    });

    it('reports its dismissal rather than hiding itself', () => {
      // Local state would satisfy "shown once" for one session and fail the
      // criterion that matters, which is that it does not come back after a
      // restart. The component tells the store and the store writes it down.
      const onAcknowledge = jest.fn();
      renderTokens({
        isDecimalPlacesNoticeAcknowledged: false,
        onAcknowledgeDecimalPlacesNotice: onAcknowledge,
      });

      fireEvent.click(screen.getByTestId('decimalPlacesNotice:dismiss'));

      expect(onAcknowledge).toHaveBeenCalledTimes(1);
    });
  });
});
