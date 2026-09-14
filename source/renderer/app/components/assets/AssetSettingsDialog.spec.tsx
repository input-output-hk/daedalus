import '@testing-library/jest-dom';

import React from 'react';
import noop from 'lodash/noop';
import { cleanup, fireEvent, screen, waitFor } from '@testing-library/react';

import createTestBed from 'tests/_utils/TestBed';
import {
  withDecimalPlacesToken,
  zeroDecimalPlacesToken,
} from 'tests/mocks/asset';

import AssetSettingsDialog from './AssetSettingsDialog';

const openDialogFor = async (asset, onRefresh?: (asset: any) => void) => {
  createTestBed(
    <AssetSettingsDialog
      asset={asset}
      onSubmit={noop}
      onCancel={noop}
      onRefresh={onRefresh}
    />
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

  describe('the refresh control', () => {
    it('asks about the one token the dialog is open on', async () => {
      const onRefresh = jest.fn();
      const asset = {
        ...withDecimalPlacesToken,
        decimals: null,
        recommendedDecimals: 6,
        recommendedDecimalsVerified: false,
      };
      await openDialogFor(asset, onRefresh);

      fireEvent.click(screen.getByTestId('refresh-metadata'));

      expect(onRefresh).toHaveBeenCalledTimes(1);
      expect(onRefresh).toHaveBeenCalledWith(asset);
    });

    it('is offered for a token whose published value verified too', async () => {
      await openDialogFor(
        {
          ...withDecimalPlacesToken,
          decimals: 6,
          recommendedDecimals: 6,
          recommendedDecimalsVerified: true,
        },
        jest.fn()
      );
      expect(screen.getByTestId('refresh-metadata')).toBeInTheDocument();
    });

    it('keeps rendering the cached row when a refresh answers nothing', async () => {
      // Offline behaves as it does everywhere else in this design: the control
      // does nothing visible and the row stays.
      const onRefresh = jest.fn();
      await openDialogFor(
        {
          ...withDecimalPlacesToken,
          decimals: null,
          recommendedDecimals: 6,
          recommendedDecimalsVerified: false,
        },
        onRefresh
      );

      fireEvent.click(screen.getByTestId('refresh-metadata'));

      expect(screen.getByText('Number of decimal places')).toBeInTheDocument();
      expect(screen.getByTestId('unverified-decimals')).toBeInTheDocument();
      expect(screen.queryByRole('alert')).not.toBeInTheDocument();
    });

    it('is absent where nothing can act on it', async () => {
      await openDialogFor({
        ...withDecimalPlacesToken,
        decimals: null,
        recommendedDecimals: 6,
        recommendedDecimalsVerified: false,
      });
      expect(screen.queryByTestId('refresh-metadata')).not.toBeInTheDocument();
    });
  });

  describe('the advisory beside the decimal places field', () => {
    const sentence =
      'This token’s issuer publishes 6 decimal places. That figure could not be checked against the token’s minting policy, so Daedalus does not apply it on its own.';

    it('appears for a published value that could not be verified', async () => {
      await openDialogFor({
        ...withDecimalPlacesToken,
        decimals: null,
        recommendedDecimals: 6,
        recommendedDecimalsVerified: false,
      });
      expect(screen.getByTestId('unverified-decimals')).toHaveTextContent(
        sentence
      );
    });

    it('does not appear for a published value that verified', async () => {
      await openDialogFor({
        ...withDecimalPlacesToken,
        decimals: 6,
        recommendedDecimals: 6,
        recommendedDecimalsVerified: true,
      });
      expect(
        screen.queryByTestId('unverified-decimals')
      ).not.toBeInTheDocument();
    });

    it('does not appear when the issuer published no decimal places', async () => {
      await openDialogFor({
        ...withDecimalPlacesToken,
        decimals: null,
        recommendedDecimals: null,
        recommendedDecimalsVerified: false,
      });
      expect(
        screen.queryByTestId('unverified-decimals')
      ).not.toBeInTheDocument();
    });

    it('appears for an unverified published zero', async () => {
      // The disagreement verdict is suppressed for this combination, which is
      // why the sentence has its own condition rather than reusing it.
      await openDialogFor({
        ...withDecimalPlacesToken,
        decimals: null,
        recommendedDecimals: 0,
        recommendedDecimalsVerified: false,
      });
      expect(screen.getByTestId('unverified-decimals')).toHaveTextContent(
        'publishes 0 decimal places'
      );
    });

    it('appears whether or not the user has already chosen a value', async () => {
      await openDialogFor({
        ...withDecimalPlacesToken,
        decimals: 2,
        recommendedDecimals: 6,
        recommendedDecimalsVerified: false,
      });
      expect(screen.getByTestId('unverified-decimals')).toHaveTextContent(
        sentence
      );
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
