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

const openDialogFor = async (asset) => {
  createTestBed(
    <AssetSettingsDialog asset={asset} onSubmit={noop} onCancel={noop} />
  );
  await waitFor(() => screen.getByText('Number of decimal places'));
};

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

  describe('when the published decimal places were verified', () => {
    it('puts a setting that contradicts them plainly', async () => {
      await openDialogFor({
        ...withDecimalPlacesToken,
        decimals: 2,
        recommendedDecimals: 6,
        recommendedDecimalsVerified: true,
      });
      expect(screen.getByTestId('warning-icon')).toHaveAttribute(
        'aria-label',
        'You are not using the recommended decimal place configuration for this native token.'
      );
    });

    it('says nothing when the setting agrees with them', async () => {
      await openDialogFor({
        ...withDecimalPlacesToken,
        decimals: 6,
        recommendedDecimals: 6,
        recommendedDecimalsVerified: true,
      });
      expect(screen.queryByTestId('warning-icon')).not.toBeInTheDocument();
    });
  });

  describe('when the published decimal places could not be verified', () => {
    it('puts a setting that contradicts them more weakly', async () => {
      await openDialogFor({
        ...withDecimalPlacesToken,
        decimals: 2,
        recommendedDecimals: 6,
        recommendedDecimalsVerified: false,
      });
      expect(screen.getByTestId('warning-icon')).toHaveAttribute(
        'aria-label',
        'Your setting differs from the 6 decimal places this token’s issuer publishes. That figure could not be checked against the token’s minting policy.'
      );
    });

    it('offers them rather than recommending them when there is no setting', async () => {
      await openDialogFor({
        ...withDecimalPlacesToken,
        decimals: null,
        recommendedDecimals: 6,
        recommendedDecimalsVerified: false,
      });
      expect(screen.getByTestId('warning-icon')).toHaveAttribute(
        'aria-label',
        'This token’s issuer publishes 6 decimal places. That figure could not be checked against the token’s minting policy, so it is offered here rather than applied.'
      );
    });
  });
});
