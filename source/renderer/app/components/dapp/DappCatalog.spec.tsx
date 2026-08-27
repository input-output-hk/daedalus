import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import StoryDecorator from '../../../../../storybook/stories/_support/StoryDecorator';
import translations from '../../i18n/locales/en-US.json';
import DappCatalog from './DappCatalog';
import type { DappCatalogProps } from './DappCatalog';

const entry = {
  id: 'example',
  name: 'Example dApp',
  description: 'A locally described dApp.',
  iconAsset: 'cardano',
};

const renderCatalog = (overrides: Partial<DappCatalogProps> = {}) => {
  const props: DappCatalogProps = {
    entries: [entry],
    available: true,
    ready: true,
    isOpen: false,
    isLaunching: false,
    onLaunch: jest.fn(),
    onClose: jest.fn(),
    ...overrides,
  };

  const result = render(
    <StoryDecorator>
      <IntlProvider locale="en-US" messages={translations}>
        <DappCatalog {...props} />
      </IntlProvider>
    </StoryDecorator>
  );
  return { ...result, props };
};

describe('DappCatalog', () => {
  afterEach(cleanup);

  it('shows only the localized unavailable state when unavailable', () => {
    renderCatalog({ available: false });

    expect(
      screen.getByText(
        'The preferred dApp catalog is unavailable in this Daedalus build.'
      )
    ).toBeVisible();
    expect(screen.queryByRole('list')).not.toBeInTheDocument();
    expect(screen.queryByRole('button')).not.toBeInTheDocument();
    expect(screen.queryByText('Example dApp')).not.toBeInTheDocument();
    expect(screen.queryByRole('note')).not.toBeInTheDocument();
  });

  it('shows the prominent compatibility disclaimer whenever available', () => {
    renderCatalog();

    expect(screen.getByRole('note')).toHaveTextContent(
      'compatibility-tested by Daedalus'
    );
    expect(screen.getByRole('note')).toHaveTextContent(
      'does not mean they have been security audited or endorsed'
    );
  });

  it('disables launch until ready and launches only the selected local entry', () => {
    const onLaunch = jest.fn();
    const { rerender } = renderCatalog({ ready: false, onLaunch });
    const launch = screen.getByRole('button', { name: 'Launch' });

    expect(launch).toBeDisabled();
    fireEvent.click(launch);
    expect(onLaunch).not.toHaveBeenCalled();

    rerender(
      <StoryDecorator>
        <IntlProvider locale="en-US" messages={translations}>
          <DappCatalog
            entries={[entry]}
            available
            ready
            isOpen={false}
            isLaunching={false}
            onLaunch={onLaunch}
            onClose={jest.fn()}
          />
        </IntlProvider>
      </StoryDecorator>
    );
    fireEvent.click(screen.getByRole('button', { name: 'Launch' }));
    expect(onLaunch).toHaveBeenCalledWith('example');
  });

  it('offers close instead of launch while a session is open', () => {
    const onClose = jest.fn();
    renderCatalog({ isOpen: true, onClose });

    expect(
      screen.queryByRole('button', { name: 'Launch' })
    ).not.toBeInTheDocument();
    fireEvent.click(screen.getByRole('button', { name: 'Close dApp' }));
    expect(onClose).toHaveBeenCalledTimes(1);
  });

  it('renders unknown icon keys and caller strings without loading or executing them', () => {
    const { container } = renderCatalog({
      entries: [
        {
          ...entry,
          name: '<script>alert(1)</script>',
          iconAsset: 'https://remote.test/icon.svg',
        },
      ],
    });

    expect(screen.getByText('<script>alert(1)</script>')).toBeVisible();
    expect(container.querySelector('script')).toBeNull();
    expect(container.querySelector('img')).toBeNull();
    expect(container.querySelector('svg')).toBeNull();
  });
});
