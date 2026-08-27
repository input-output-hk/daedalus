import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import StoryDecorator from '../../../../../../storybook/stories/_support/StoryDecorator';
import translations from '../../../i18n/locales/en-US.json';
import type {
  CollateralPreference,
  CollateralState,
} from '../../../../../common/types/collateral.types';
import CollateralPanel, { CollateralPanelProps } from './CollateralPanel';

const preference = (state: CollateralState): CollateralPreference => ({
  schemaVersion: 1,
  walletId: 'ab'.repeat(20),
  networkGenesis: 'cd'.repeat(32),
  targetLovelace: '5000000',
  preferredInputs:
    state === 'checking' || state === 'not-ready' || state === 'preparing'
      ? []
      : [{ transactionId: 'ef'.repeat(32), index: 0 }],
  generation: 1,
  state,
});

const renderPanel = (overrides: Partial<CollateralPanelProps> = {}) => {
  const props: CollateralPanelProps = {
    preference: preference('ready'),
    corrupt: false,
    busy: false,
    failed: false,
    onPrepare: jest.fn(),
    onCancelPreparation: jest.fn(),
    onClear: jest.fn(),
    onRepair: jest.fn(),
    ...overrides,
  };
  render(
    <StoryDecorator>
      <IntlProvider locale="en-US" messages={translations}>
        <CollateralPanel {...props} />
      </IntlProvider>
    </StoryDecorator>
  );
  return props;
};

describe('CollateralPanel', () => {
  afterEach(cleanup);

  it.each([
    ['checking', 'Checking preferred collateral'],
    ['ready', 'Preferred collateral is ready'],
    ['not-ready', 'No suitable preferred collateral'],
    ['preparing', 'normal confirmed self-transfer'],
    ['in-use', 'pending transaction'],
    ['will-be-spent', 'will spend the preferred collateral'],
    ['charged', 'was charged'],
    ['stale', 'no longer available'],
  ] as const)('represents the %s state', (state, copy) => {
    renderPanel({ preference: preference(state) });
    expect(screen.getByRole('status')).toHaveTextContent(copy);
  });

  it('explains the preferred 5 ADA convention without implying reservation', () => {
    renderPanel();
    expect(screen.getByText(/5 ADA/)).toHaveTextContent(
      'compatibility convention, not a protocol maximum'
    );
    expect(screen.getByText(/5 ADA/)).toHaveTextContent(
      'does not reserve the output'
    );
  });

  it('starts preparation and clearing only through explicit actions', () => {
    const onPrepare = jest.fn();
    renderPanel({
      preference: preference('not-ready'),
      onPrepare,
    });
    fireEvent.click(screen.getByRole('button', { name: 'Prepare collateral' }));
    expect(onPrepare).toHaveBeenCalledTimes(1);

    cleanup();
    const onClear = jest.fn();
    renderPanel({ preference: preference('ready'), onClear });
    fireEvent.click(screen.getByRole('button', { name: 'Clear preference' }));
    expect(onClear).toHaveBeenCalledTimes(1);
  });
});
