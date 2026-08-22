import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../../source/renderer/app/i18n/locales/en-US.json';
import DRepDetailAnchorSection from '../../../source/renderer/app/components/governance/drep-detail/DRepDetailAnchorSection';
import { isHttpsUrl } from '../../../source/renderer/app/utils/governance/isHttpsUrl';

/**
 * Every string on a DRep detail page is written by the DRep.
 *
 * The name, the objectives, the reference labels and the reference URIs all
 * come from a document the DRep published and this wallet fetched. The only
 * thing standing between that and the renderer is a Blake2b digest check, which
 * proves the document is the one the chain points at, not that its contents are
 * benign: a DRep can register an anchor whose hash matches a document full of
 * markup and hostile URLs.
 *
 * These tests hold the two properties the page relies on. Text supplied by a
 * DRep is rendered as text, never as markup, which React gives us as long as
 * nothing reaches for dangerouslySetInnerHTML. And a URL supplied by a DRep
 * becomes a link only when its scheme is https, which is a whitelist rather
 * than a list of the schemes someone thought to block.
 */

const HOSTILE_MARKUP = '<script>window.__pwned = true;</script>';
const HOSTILE_IMG = '<img src=x onerror="window.__pwned = true">';
// Right-to-left override: renders a URL as though it pointed somewhere else.
const BIDI_SPOOF = 'https://example.org/‮gnp.exe';

const hostileMetadata = {
  objectives: `Objectives ${HOSTILE_MARKUP}`,
  motivations: `Motivations ${HOSTILE_IMG}`,
  qualifications: '</dd><dd>escaped out of the field</dd>',
  paymentAddress: 'addr1<script>alert(1)</script>',
  references: [
    { type: 'link', label: 'Runs script', uri: 'javascript:window.__pwned=1' },
    {
      type: 'link',
      label: 'Inline page',
      uri: 'data:text/html,<script>1</script>',
    },
    { type: 'link', label: 'Local file', uri: 'file:///etc/passwd' },
    { type: 'link', label: 'Scheme relative', uri: '//evil.example.org/x' },
    { type: 'link', label: HOSTILE_MARKUP, uri: 'https://example.org/ok' },
    { type: 'link', label: 'Bidi spoof', uri: BIDI_SPOOF },
  ],
};

const renderSection = (
  overrides: {
    anchorUrl?: string;
    onOpenExternalLink?: (url: string) => void;
    verifiedName?: string | null;
  } = {}
) =>
  render(
    <IntlProvider locale="en-US" messages={translations}>
      <DRepDetailAnchorSection
        anchor={{
          url: overrides.anchorUrl ?? 'https://example.org/drep.json',
          hash: 'a'.repeat(64),
        }}
        verifiedName={
          overrides.verifiedName === undefined
            ? `Name ${HOSTILE_MARKUP}`
            : overrides.verifiedName
        }
        metadata={hostileMetadata as any}
        onOpenExternalLink={overrides.onOpenExternalLink ?? jest.fn()}
      />
    </IntlProvider>
  );

describe('isHttpsUrl', () => {
  it('admits https and nothing else', () => {
    expect(isHttpsUrl('https://example.org/a')).toBe(true);
    expect(isHttpsUrl('http://example.org/a')).toBe(false);
    expect(isHttpsUrl('javascript:alert(1)')).toBe(false);
    expect(isHttpsUrl('data:text/html,<script>1</script>')).toBe(false);
    expect(isHttpsUrl('file:///etc/passwd')).toBe(false);
    expect(isHttpsUrl('vbscript:msgbox(1)')).toBe(false);
    // No scheme at all, so nothing to open.
    expect(isHttpsUrl('//evil.example.org/x')).toBe(false);
    expect(isHttpsUrl('example.org')).toBe(false);
    expect(isHttpsUrl('')).toBe(false);
  });

  it('is not fooled by a scheme that merely starts with https', () => {
    expect(isHttpsUrl('httpsx://example.org')).toBe(false);
    // Case is normalised by the URL parser, so this really is https.
    expect(isHttpsUrl('HTTPS://example.org')).toBe(true);
  });

  it('rejects a javascript URL dressed up with whitespace or case', () => {
    expect(isHttpsUrl('  javascript:alert(1)')).toBe(false);
    expect(isHttpsUrl('JaVaScRiPt:alert(1)')).toBe(false);
    expect(isHttpsUrl('java\nscript:alert(1)')).toBe(false);
  });
});

describe('DRep metadata rendering is inert', () => {
  afterEach(() => {
    cleanup();
    delete (window as any).__pwned;
  });

  it('creates no script element from any DRep-supplied string', () => {
    const { container } = renderSection();

    expect(container.querySelectorAll('script')).toHaveLength(0);
    expect(container.querySelectorAll('img')).toHaveLength(0);
    expect((window as any).__pwned).toBeUndefined();
  });

  it('renders markup in a prose field as the literal text it is', () => {
    renderSection();

    expect(
      screen.getByText(`Objectives ${HOSTILE_MARKUP}`)
    ).toBeInTheDocument();
    expect(screen.getByText(`Motivations ${HOSTILE_IMG}`)).toBeInTheDocument();
  });

  it('renders markup in a reference label as text rather than as a tag', () => {
    renderSection();

    expect(screen.getByText(HOSTILE_MARKUP)).toBeInTheDocument();
  });

  it('gives no href to a javascript, data, file or scheme-relative URI', () => {
    const { container } = renderSection();

    const hrefs = Array.from(container.querySelectorAll('a')).map((a) =>
      a.getAttribute('href')
    );
    hrefs.forEach((href) => {
      expect(href).toMatch(/^https:/);
    });
    expect(hrefs).not.toContain('javascript:window.__pwned=1');
  });

  it('still shows a rejected URI, so the DRep cannot hide where it points', () => {
    // Withholding the link is the protection; withholding the text would let a
    // DRep publish a reference nobody can see to judge.
    renderSection();

    expect(screen.getByText('Runs script')).toBeInTheDocument();
    expect(screen.getByText('Local file')).toBeInTheDocument();
  });

  it('never hands a non-https URI to the external link opener', () => {
    const onOpenExternalLink = jest.fn();
    const { container } = renderSection({ onOpenExternalLink });

    Array.from(container.querySelectorAll('a')).forEach((anchor) => {
      anchor.dispatchEvent(new MouseEvent('click', { bubbles: true }));
    });

    onOpenExternalLink.mock.calls.forEach(([url]) => {
      expect(isHttpsUrl(url)).toBe(true);
    });
  });

  it('marks every followable DRep link as leaving the wallet', () => {
    // Until Daedalus warns before following a third-party link, the icon is
    // the only signal a reader gets. A reference shows its label rather than
    // its URI whenever one is supplied, so the words say nothing about the
    // destination.
    const { container } = renderSection();

    const links = Array.from(container.querySelectorAll('a'));
    expect(links.length).toBeGreaterThan(0);
    links.forEach((link) => {
      expect(link.querySelector('svg')).not.toBeNull();
    });
  });

  it('keeps the destination inspectable on every followable link', () => {
    // A real anchor rather than a span, so the browser's own defences survive:
    // the status bar on hover, and copy link address.
    const { container } = renderSection();

    Array.from(container.querySelectorAll('a')).forEach((link) => {
      expect(link.getAttribute('href')).toMatch(/^https:/);
      expect(link.getAttribute('rel')).toContain('noopener');
    });
  });

  it('does not link an anchor URL that is not https', () => {
    const { container } = renderSection({
      anchorUrl: 'javascript:window.__pwned=1',
    });

    Array.from(container.querySelectorAll('a')).forEach((anchor) => {
      expect(anchor.getAttribute('href')).toMatch(/^https:/);
    });
    expect((window as any).__pwned).toBeUndefined();
  });
});
