import '@testing-library/jest-dom';

import React from 'react';
import noop from 'lodash/noop';
import { cleanup, screen, waitFor } from '@testing-library/react';

import createTestBed from 'tests/_utils/TestBed';
import {
  withDecimalPlacesToken,
  zeroDecimalPlacesToken,
} from 'tests/mocks/asset';

import WalletToken from './WalletToken';

const defaultWalletProps = {
  isFavorite: false,
  isInsertingAsset: false,
  isLoading: false,
  isRemovingAsset: false,
  anyAssetWasHovered: false,
  assetSettingsDialogWasOpened: false,
  onAssetSettings: noop,
  onCopyAssetParam: noop,
  onOpenAssetSend: noop,
  onToggleFavorite: noop,
};

describe('WalletToken', () => {
  afterEach(() => cleanup());

  it('should not show a warning when an asset is set to zero recommended decimal places', async () => {
    createTestBed(
      <WalletToken asset={zeroDecimalPlacesToken} {...defaultWalletProps} />
    );
    await waitFor(() => screen.getByText(zeroDecimalPlacesToken.policyId));
    expect(screen.queryByTestId('warning-icon')).not.toBeInTheDocument();
  });

  it('should show a warning when an asset is not set to the recommended decimal places', async () => {
    createTestBed(
      <WalletToken asset={withDecimalPlacesToken} {...defaultWalletProps} />
    );
    await waitFor(() => screen.getByText(withDecimalPlacesToken.policyId));
    expect(screen.queryAllByTestId('warning-icon').length).toEqual(2);
  });
});
