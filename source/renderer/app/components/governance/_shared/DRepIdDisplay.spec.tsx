import React from 'react';
import { bech32 } from 'bech32';
import { IntlProvider } from 'react-intl';
import { ThemeProvider } from 'react-polymorph/lib/components/ThemeProvider';
import { SimpleSkins } from 'react-polymorph/lib/skins/simple';
import { SimpleDefaults } from 'react-polymorph/lib/themes/simple';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../../i18n/locales/en-US.json';
import jaTranslations from '../../../i18n/locales/ja-JP.json';
import { daedalusTheme } from '../../../themes/daedalus';
import { themeOverrides } from '../../../themes/overrides';
import { logger } from '../../../utils/logging';
import DRepIdDisplay from './DRepIdDisplay';
import type { DRepIdDisplayVariant } from './DRepIdDisplay';

const CIP129 = 'drep1yg7svuv02gh9j2q574jv06l4xnzwyp63effljze28qe993caj8ras';
const CIP105 = 'drep_vkh185r8rr6j9evjs984vnr7haf5cn3qw5w220usk23cxffvw6msqtt';
const UNDECODABLE = 'drep1yg7s8vuv87f8a8f5d0m9yk4p5xqw6r4s3t2u1v9w8x7y6z5a4b';

const credentialOf = (id: string): string => {
  const bytes = bech32.fromWords(bech32.decode(id).words);
  const credential = bytes.length === 29 ? bytes.slice(1) : bytes;
  return Buffer.from(credential).toString('hex');
};

// CIP-105 is deprecated and hidden behind a disclosure, so tests that assert on
// it reveal it first. renderDisplay does that by default; pass
// `revealCip105: false` to exercise the collapsed state.
const renderDisplay = (
  drepId: string,
  variant: DRepIdDisplayVariant,
  locale = 'en-US',
  { revealCip105 = true }: { revealCip105?: boolean } = {}
) => {
  const view = renderCollapsed(drepId, variant, locale);
  if (revealCip105) {
    const toggle = screen.queryByText(
      (text) => text.includes('CIP-105') && /Show|表示/.test(text)
    );
    if (toggle) fireEvent.click(toggle);
  }
  return view;
};

const renderCollapsed = (
  drepId: string,
  variant: DRepIdDisplayVariant,
  locale = 'en-US'
) =>
  render(
    <ThemeProvider
      theme={daedalusTheme}
      skins={SimpleSkins}
      variables={SimpleDefaults}
      themeOverrides={themeOverrides}
    >
      <IntlProvider
        locale={locale}
        messages={locale === 'ja-JP' ? jaTranslations : translations}
      >
        <DRepIdDisplay drepId={drepId} variant={variant} />
      </IntlProvider>
    </ThemeProvider>
  );

describe('DRepIdDisplay', () => {
  afterEach(() => {
    cleanup();
    delete (navigator as any).clipboard;
    jest.restoreAllMocks();
  });

  it('keeps the single truncated form and one copy button by default', () => {
    const { container } = render(
      <ThemeProvider
        theme={daedalusTheme}
        skins={SimpleSkins}
        variables={SimpleDefaults}
        themeOverrides={themeOverrides}
      >
        <IntlProvider locale="en-US" messages={translations}>
          <DRepIdDisplay drepId={CIP129} />
        </IntlProvider>
      </ThemeProvider>
    );

    expect(container.querySelectorAll('code')).toHaveLength(1);
    expect(screen.getByText('drep1yg7…aj8ras')).toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: 'Copy DRep ID' })
    ).toBeInTheDocument();
    expect(screen.queryByText('(CIP-105)')).not.toBeInTheDocument();
  });

  it('stacks both truncated forms with a copy button each', () => {
    const { container } = renderDisplay(CIP129, 'stacked');

    expect(container.querySelectorAll('code')).toHaveLength(2);
    expect(screen.getByText('drep1yg7…aj8ras')).toBeInTheDocument();
    expect(screen.getByText('drep_vkh…6msqtt')).toBeInTheDocument();
    expect(screen.getByText('(CIP-105)')).toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: 'Copy CIP-129 DRep ID' })
    ).toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: 'Copy CIP-105 DRep ID' })
    ).toBeInTheDocument();
  });

  it('renders both forms in full with distinct per-form aria labels', () => {
    const { container } = renderDisplay(CIP129, 'full');

    const codes = Array.from(container.querySelectorAll('code'));
    expect(codes).toHaveLength(2);
    expect(codes[0]).toHaveTextContent(CIP129);
    expect(codes[1]).toHaveTextContent(CIP105);
    expect(codes[0].getAttribute('aria-label')).toBe(
      `CIP-129 DRep ID ${CIP129}`
    );
    expect(codes[1].getAttribute('aria-label')).toBe(
      `CIP-105 DRep ID ${CIP105}`
    );
  });

  it('renders both forms over the same credential bytes', () => {
    const { container } = renderDisplay(CIP129, 'full');

    const codes = Array.from(container.querySelectorAll('code'));
    expect(credentialOf(codes[0].textContent as string)).toBe(
      credentialOf(CIP129)
    );
    expect(credentialOf(codes[1].textContent as string)).toBe(
      credentialOf(CIP129)
    );
  });

  it('omits the second row when the id does not decode', () => {
    const { container } = renderDisplay(UNDECODABLE, 'full');

    expect(container.querySelectorAll('code')).toHaveLength(1);
    expect(screen.queryByText('(CIP-105)')).not.toBeInTheDocument();
    expect(
      screen.queryByRole('button', { name: 'Copy CIP-105 DRep ID' })
    ).not.toBeInTheDocument();
  });

  it('copies exactly the form each button labels', () => {
    const writeText = jest.fn(async () => undefined);
    Object.defineProperty(navigator, 'clipboard', {
      configurable: true,
      value: { writeText },
    });

    renderDisplay(CIP129, 'full');

    fireEvent.click(
      screen.getByRole('button', { name: 'Copy CIP-129 DRep ID' })
    );
    expect(writeText).toHaveBeenLastCalledWith(CIP129);

    fireEvent.click(
      screen.getByRole('button', { name: 'Copy CIP-105 DRep ID' })
    );
    expect(writeText).toHaveBeenLastCalledWith(CIP105);
  });

  it('logs no id when the clipboard API is unavailable, for either form', () => {
    const warn = jest.spyOn(logger, 'warn').mockImplementation(() => undefined);

    renderDisplay(CIP129, 'full');
    fireEvent.click(
      screen.getByRole('button', { name: 'Copy CIP-129 DRep ID' })
    );
    fireEvent.click(
      screen.getByRole('button', { name: 'Copy CIP-105 DRep ID' })
    );

    expect(warn).toHaveBeenCalledTimes(2);
    warn.mock.calls.forEach(([message, payload]) => {
      expect(message).toBe('DRepIdDisplay: clipboard API is unavailable');
      expect(Object.keys(payload as object)).toEqual(['drepIdLength']);
      const serialized = JSON.stringify(payload);
      expect(serialized).not.toContain(CIP129);
      expect(serialized).not.toContain(CIP105);
    });
  });

  it('logs no id when a copy rejects, for either form', async () => {
    const writeText = jest.fn(() => Promise.reject(new Error('denied')));
    Object.defineProperty(navigator, 'clipboard', {
      configurable: true,
      value: { writeText },
    });
    const warn = jest.spyOn(logger, 'warn').mockImplementation(() => undefined);

    renderDisplay(CIP129, 'full');
    fireEvent.click(
      screen.getByRole('button', { name: 'Copy CIP-129 DRep ID' })
    );
    fireEvent.click(
      screen.getByRole('button', { name: 'Copy CIP-105 DRep ID' })
    );
    await Promise.resolve();
    await Promise.resolve();

    expect(warn).toHaveBeenCalledTimes(2);
    warn.mock.calls.forEach(([message, payload]) => {
      expect(message).toBe('DRepIdDisplay: failed to copy DRep ID');
      expect(Object.keys(payload as object).sort()).toEqual([
        'drepIdLength',
        'error',
      ]);
      const serialized = JSON.stringify(payload, (_key, val) =>
        val instanceof Error ? `${val.message} ${val.stack}` : val
      );
      expect(serialized).not.toContain(CIP129);
      expect(serialized).not.toContain(CIP105);
    });
  });

  it('renders the stacked form with the ja-JP copy labels', () => {
    renderDisplay(CIP129, 'stacked', 'ja-JP');

    expect(
      screen.getByRole('button', { name: '!!!CIP-129 DRep IDをコピー' })
    ).toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: '!!!CIP-105 DRep IDをコピー' })
    ).toBeInTheDocument();
  });
});

describe('DRepIdDisplay deprecated CIP-105 form', () => {
  afterEach(cleanup);

  it('hides the CIP-105 form until it is asked for', () => {
    // CIP-105 is deprecated, so it does not occupy the reading order by
    // default; it stays reachable for matching ids recorded in the older form.
    renderCollapsed(CIP129, 'full');

    expect(screen.queryByText('(CIP-105)')).toBeNull();
    expect(screen.getByText('Show deprecated CIP-105 ID')).toBeInTheDocument();
  });

  it('reveals and hides it again', () => {
    renderCollapsed(CIP129, 'full');

    fireEvent.click(screen.getByText('Show deprecated CIP-105 ID'));
    expect(screen.getByText('(CIP-105)')).toBeInTheDocument();

    fireEvent.click(screen.getByText('Hide deprecated CIP-105 ID'));
    expect(screen.queryByText('(CIP-105)')).toBeNull();
  });

  it('reports its expanded state to assistive technology', () => {
    renderCollapsed(CIP129, 'full');
    const toggle = screen.getByText('Show deprecated CIP-105 ID');

    expect(toggle.getAttribute('aria-expanded')).toBe('false');
    fireEvent.click(toggle);
    expect(
      screen
        .getByText('Hide deprecated CIP-105 ID')
        .getAttribute('aria-expanded')
    ).toBe('true');
  });

  it('offers no disclosure when the id does not decode', () => {
    renderCollapsed(UNDECODABLE, 'full');

    expect(screen.queryByText('Show deprecated CIP-105 ID')).toBeNull();
  });

  it('never offers it on the single-line form, which shows one id only', () => {
    renderCollapsed(CIP129, 'single');

    expect(screen.queryByText('Show deprecated CIP-105 ID')).toBeNull();
  });
});
