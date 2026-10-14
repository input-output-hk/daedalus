import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import StoryDecorator from '../../../../../storybook/stories/_support/StoryDecorator';
import translations from '../../i18n/locales/en-US.json';
import japaneseTranslations from '../../i18n/locales/ja-JP.json';
import DappCatalog from './DappCatalog';
import type { DappCatalogProps } from './DappCatalog';

const entry = {
  id: 'example',
  name: 'Example dApp',
  description: 'A locally described dApp.',
  iconAsset: 'cardano',
};

const locales = {
  'en-US': translations,
  'ja-JP': japaneseTranslations,
};

const renderCatalog = (
  overrides: Partial<DappCatalogProps> = {},
  locale: keyof typeof locales = 'en-US'
) => {
  const props: DappCatalogProps = {
    entries: [entry],
    available: true,
    ready: true,
    isOpen: false,
    launchingId: null,
    launchFailed: false,
    onLaunch: jest.fn(),
    onClose: jest.fn(),
    ...overrides,
  };

  const result = render(
    <StoryDecorator>
      <IntlProvider locale={locale} messages={locales[locale]}>
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

    expect(screen.getByRole('status')).toBeVisible();
    expect(screen.queryByRole('list')).not.toBeInTheDocument();
    expect(screen.queryByRole('button')).not.toBeInTheDocument();
    expect(screen.queryByText('Example dApp')).not.toBeInTheDocument();
    expect(screen.queryByRole('note')).not.toBeInTheDocument();
  });

  it.each([
    ['en-US', 'The dApp could not be opened.'],
    ['ja-JP', 'dAppを開けませんでした。'],
  ] as const)(
    'shows one accessible launch failure in %s',
    (locale, message) => {
      const { rerender, props } = renderCatalog({ launchFailed: true }, locale);

      expect(screen.getAllByRole('alert')).toHaveLength(1);
      expect(screen.getByRole('alert')).toBeVisible();
      expect(screen.getByRole('alert').textContent).toBe(message);
      expect(screen.queryByRole('status')).not.toBeInTheDocument();

      rerender(
        <StoryDecorator>
          <IntlProvider locale={locale} messages={locales[locale]}>
            <DappCatalog {...props} launchFailed={false} isOpen />
          </IntlProvider>
        </StoryDecorator>
      );

      expect(screen.queryByRole('alert')).not.toBeInTheDocument();
      expect(screen.getByRole('status')).toBeVisible();
    }
  );

  it.each(['en-US', 'ja-JP'] as const)(
    'does not show a failure before launch in %s',
    (locale) => {
      renderCatalog({}, locale);

      expect(screen.queryByRole('alert')).not.toBeInTheDocument();
    }
  );

  it.each(['en-US', 'ja-JP'] as const)(
    'hides a prior failure when the catalog is unavailable in %s',
    (locale) => {
      renderCatalog({ available: false, launchFailed: true }, locale);

      expect(screen.queryByRole('alert')).not.toBeInTheDocument();
      expect(screen.getByRole('status')).toBeVisible();
      expect(screen.queryByRole('list')).not.toBeInTheDocument();
    }
  );

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
            launchingId={null}
            launchFailed={false}
            onLaunch={onLaunch}
            onClose={jest.fn()}
          />
        </IntlProvider>
      </StoryDecorator>
    );
    fireEvent.click(screen.getByRole('button', { name: 'Launch' }));
    expect(onLaunch).toHaveBeenCalledWith('example');
  });

  it('keeps launch actions available and scopes launching text to one dApp', () => {
    const secondEntry = { ...entry, id: 'second', name: 'Second dApp' };
    const onClose = jest.fn();
    renderCatalog({
      entries: [entry, secondEntry],
      isOpen: true,
      launchingId: 'example',
      onClose,
    });

    expect(screen.getByRole('button', { name: 'Launching…' })).toBeDisabled();
    expect(screen.getByRole('button', { name: 'Launch' })).toBeDisabled();
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
