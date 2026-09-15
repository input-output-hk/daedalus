/**
 * The token row header asking for a logo, and what it does with the answer.
 *
 * The channel client is mocked: what is under test is which rows ask and what
 * they render, not the transport, which has its own suite.
 *
 * The tree is built here rather than through `createTestBed`, because one case
 * re-renders the header and that helper wraps its argument in providers it does
 * not export, so a re-render through it would replace the provider tree instead
 * of the component under it.
 */
import '@testing-library/jest-dom';

import React from 'react';
import noop from 'lodash/noop';
import { cleanup, render, screen, waitFor } from '@testing-library/react';

import { TestDecorator } from 'tests/_utils/TestDecorator';
import { zeroDecimalPlacesToken } from 'tests/mocks/asset';

import {
  BrowserLocalStorageBridge,
  DiscreetModeFeatureProvider,
} from '../../../../features';

import WalletTokenHeader from './WalletTokenHeader';

jest.mock('../../../../ipc/assetMetadataChannel', () => ({
  requestAssetImageUrl: jest.fn(),
}));

const { requestAssetImageUrl } = jest.requireMock(
  '../../../../ipc/assetMetadataChannel'
);

const LOGO = 'data:image/png;base64,iVBORw==';
const { policyId, assetName, fingerprint } = zeroDecimalPlacesToken;
const SUBJECT = `${policyId}${assetName}`;

const renderHeader = (hasImage: boolean) => {
  // A fresh element each render. `WalletTokenHeader` is an `observer`, so
  // mobx-react gives it a shallow prop comparison and an identical element with
  // a mutated asset would be skipped. In the application the row is a new plain
  // object on every container render for the same reason.
  const tree = (rowHasImage: boolean) => (
    <TestDecorator>
      <BrowserLocalStorageBridge>
        <DiscreetModeFeatureProvider>
          <WalletTokenHeader
            asset={{ ...zeroDecimalPlacesToken, hasImage: rowHasImage }}
            anyAssetWasHovered={false}
            assetSettingsDialogWasOpened={false}
            isExpanded={false}
            isFavorite={false}
            isLoading={false}
            hasWarning={false}
            onClick={noop}
            onCopyAssetParam={noop}
          />
        </DiscreetModeFeatureProvider>
      </BrowserLocalStorageBridge>
    </TestDecorator>
  );
  const { rerender } = render(tree(hasImage));
  return {
    setHasImage: (next: boolean) => rerender(tree(next)),
  };
};

describe('WalletTokenHeader', () => {
  afterEach(() => cleanup());

  it('renders the logo of a subject the cache holds one for', async () => {
    requestAssetImageUrl.mockResolvedValue(LOGO);
    renderHeader(true);

    expect(await screen.findByTestId('logo')).toHaveAttribute('src', LOGO);
    expect(requestAssetImageUrl).toHaveBeenCalledWith(SUBJECT);
  });

  it('asks for nothing and renders nothing for a row with no logo', async () => {
    requestAssetImageUrl.mockResolvedValue(LOGO);
    renderHeader(false);

    await waitFor(() => screen.getByText(fingerprint));
    expect(requestAssetImageUrl).not.toHaveBeenCalled();
    expect(screen.queryByTestId('logo')).not.toBeInTheDocument();
  });

  it('adds no element when the cache answers that there is no logo', async () => {
    requestAssetImageUrl.mockResolvedValue(null);
    renderHeader(true);

    await waitFor(() => expect(requestAssetImageUrl).toHaveBeenCalled());
    expect(screen.queryByTestId('logo')).not.toBeInTheDocument();
  });

  it('asks as soon as the row it is drawn from says there is a logo', async () => {
    requestAssetImageUrl.mockResolvedValue(LOGO);
    const { setHasImage } = renderHeader(false);
    await waitFor(() => screen.getByText(fingerprint));
    expect(requestAssetImageUrl).not.toHaveBeenCalled();

    // A row drawn before its metadata arrives says it has no logo. A component
    // keyed on the subject alone would never ask once that changes.
    setHasImage(true);
    expect(await screen.findByTestId('logo')).toHaveAttribute('src', LOGO);
  });
});
