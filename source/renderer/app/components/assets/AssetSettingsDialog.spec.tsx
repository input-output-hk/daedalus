import '@testing-library/jest-dom';

import React from 'react';
import noop from 'lodash/noop';
import { cleanup, screen, waitFor } from '@testing-library/react';

import createTestBed from 'tests/_utils/TestBed';
import {
  withDecimalPlacesToken,
  zeroDecimalPlacesToken,
} from 'tests/mocks/asset';

import AssetSettingsDialog from './AssetSettingsDialog';

describe('AssetSettingsDialog', () => {
  afterEach(() => cleanup());

  it('should not show a warning when an asset is set to zero recommended decimal places', async () => {
    createTestBed(
      <AssetSettingsDialog
        asset={zeroDecimalPlacesToken}
        onSubmit={noop}
        onCancel={noop}
      />
    );
    await waitFor(() => screen.getByText('Number of decimal places'));
    expect(screen.queryByTestId('warning-icon')).not.toBeInTheDocument();
  });

  it('should show a warning when an asset is not set to the recommended decimal places', async () => {
    createTestBed(
      <AssetSettingsDialog
        asset={withDecimalPlacesToken}
        onSubmit={noop}
        onCancel={noop}
      />
    );
    await waitFor(() => screen.getByText('Number of decimal places'));
    expect(screen.queryByTestId('warning-icon')).toBeInTheDocument();
  });
});
