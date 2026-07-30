import React from 'react';
import BigNumber from 'bignumber.js';
import { observable, runInAction } from 'mobx';
import { Provider } from 'mobx-react';
import { Route, Router } from 'react-router-dom';
import { createMemoryHistory } from 'history';
import { IntlProvider } from 'react-intl';
import {
  act,
  cleanup,
  fireEvent,
  render,
  screen,
} from '@testing-library/react';
import '@testing-library/jest-dom';
import { AnchorFetchErrorType } from '../../../../common/types/governance.types';
import type { VerifiedDRepAnchorContent } from '../../../../common/types/governance.types';
import { logger as rendererLogger } from '../../utils/logging';
import translations from '../../i18n/locales/en-US.json';
import jaTranslations from '../../i18n/locales/ja-JP.json';
import { ROUTES } from '../../routes-config';
import {
  GovernanceRefreshState,
  VotingPowerEnrichState,
} from '../../stores/GovernanceStore';
import type { AppDRepDirectoryEntry } from '../../stores/GovernanceStore';
import DRepDetailPage from './DRepDetailPage';

const DREP_ID = 'drep1yg7s8vuv87f8a8f5d0m9yk4p5xqw6r4s3t2u1v9w8x7y6z5a4b';
const DECODABLE_DREP_ID =
  'drep1yg7svuv02gh9j2q574jv06l4xnzwyp63effljze28qe993caj8ras';
const DECODABLE_CIP105 =
  'drep_vkh185r8rr6j9evjs984vnr7haf5cn3qw5w220usk23cxffvw6msqtt';
// The route literal lands with the route-wiring task; deriving the path from
// the directory literal keeps this harness aligned with it.
const DETAIL_PATH = `${ROUTES.GOVERNANCE.DREPS}/:drepId`;

const baseEntry: AppDRepDirectoryEntry = {
  anchor: {
    hash: '6a5e200d2f3a1020202020202020202020202020202020202020202020202020',
    url: 'https://governance-preview.example.org/dreps/1.json',
  },
  verifiedName: null,
  doNotList: false,
  drepActivity: 34,
  drepId: DREP_ID,
  status: 'active',
  votingPower: new BigNumber('23137980123456'),
};

const verifiedContent = (
  overrides: Partial<VerifiedDRepAnchorContent> = {}
): VerifiedDRepAnchorContent => ({
  givenName: 'Daedalus Test DRep',
  objectives: null,
  motivations: null,
  qualifications: null,
  references: [],
  paymentAddress: null,
  doNotList: false,
  ...overrides,
});

const verifiedState = (overrides: Partial<VerifiedDRepAnchorContent> = {}) =>
  new Map([
    [
      DREP_ID,
      {
        state: 'verified',
        hash: baseEntry.anchor!.hash,
        host: 'raw.githubusercontent.com',
        content: verifiedContent(overrides),
      },
    ],
  ]);

const buildGovernanceStore = (overrides: Record<string, unknown> = {}) => ({
  anchorStateByDRepId: new Map(),
  cohortContext: {
    medianVotingPower: new BigNumber('99137980123456'),
    memberIds: new Set([DREP_ID]),
    verifiedMetadataIds: new Set([DREP_ID]),
  },
  drepIndex: new Map([[DREP_ID, baseEntry]]),
  drepList: [baseEntry],
  error: null,
  fetchAnchorContent: jest.fn(),
  lastFetchedAt: Date.now() - 60_000,
  refresh: jest.fn(),
  refreshState: GovernanceRefreshState.Loaded,
  votingPowerState: VotingPowerEnrichState.Loaded,
  ...overrides,
});

const renderPage = ({
  governanceOverrides = {},
  drepId = DREP_ID,
  isNodeInSync = true,
  locale = 'en-US',
  locationState,
  syncProgress = 100,
}: {
  governanceOverrides?: Record<string, unknown>;
  drepId?: string;
  isNodeInSync?: boolean;
  locale?: string;
  locationState?: Record<string, unknown>;
  syncProgress?: number | null;
} = {}) => {
  // Observable so the container's reaction sees the flip like the real store.
  const networkStatus = observable({ isNodeInSync, syncProgress });
  const governance = buildGovernanceStore(governanceOverrides);
  const app = { openExternalLink: jest.fn() };
  const history = createMemoryHistory({
    initialEntries: [
      {
        pathname: `${ROUTES.GOVERNANCE.DREPS}/${drepId}`,
        state: locationState,
      },
    ],
  });
  const pushSpy = jest.spyOn(history, 'push');
  const messages = locale === 'ja-JP' ? jaTranslations : translations;
  const view = render(
    <Provider stores={{ app, governance, networkStatus } as any}>
      <IntlProvider locale={locale} messages={messages}>
        <Router history={history}>
          <Route path={DETAIL_PATH} component={DRepDetailPage} />
        </Router>
      </IntlProvider>
    </Provider>
  );
  return { app, governance, history, networkStatus, pushSpy, ...view };
};

describe('DRepDetailPage', () => {
  afterEach(() => {
    cleanup();
    jest.restoreAllMocks();
  });

  it('renders the on-chain fields for a loaded entry', () => {
    renderPage();

    expect(screen.getByText('!!!DRep detail')).toBeInTheDocument();
    expect(screen.getByText('!!!Active')).toBeInTheDocument();
    expect(screen.getByText('!!!34 epochs')).toBeInTheDocument();
    expect(screen.getByText('₳ 23,137,980.123456')).toBeInTheDocument();
    expect(
      screen.getByText('!!!(23,137,980,123,456 lovelace)')
    ).toBeInTheDocument();
    expect(
      screen.getByText('!!!Vote positions are not available in this version.')
    ).toBeInTheDocument();
  });

  it('renders the anchor presence with the on-chain anchor reference label', () => {
    renderPage();

    expect(
      screen.getByText('https://governance-preview.example.org/dreps/1.json')
    ).toBeInTheDocument();
    expect(screen.getByText(baseEntry.anchor!.hash)).toBeInTheDocument();
    expect(
      screen.getByText('!!!On-chain anchor reference')
    ).toBeInTheDocument();
  });

  it('renders the anchor-absent message when no anchor is recorded', () => {
    renderPage({
      governanceOverrides: {
        drepIndex: new Map([[DREP_ID, { ...baseEntry, anchor: null }]]),
      },
    });

    expect(
      screen.getByText('!!!No anchor is recorded on-chain for this DRep.')
    ).toBeInTheDocument();
    expect(
      screen.queryByText('!!!On-chain anchor reference')
    ).not.toBeInTheDocument();
  });

  it('shows — with the unavailable tooltip when stake enrichment failed', () => {
    renderPage({
      governanceOverrides: {
        drepIndex: new Map([[DREP_ID, { ...baseEntry, votingPower: null }]]),
        votingPowerState: VotingPowerEnrichState.Failed,
      },
    });

    expect(screen.getByText('—')).toHaveAttribute(
      'title',
      '!!!Stake distribution unavailable this refresh.'
    );
  });

  it('refreshes on mount from an empty Idle store and shows the loading state', () => {
    const { governance } = renderPage({
      governanceOverrides: {
        drepIndex: new Map(),
        drepList: [],
        refreshState: GovernanceRefreshState.Idle,
      },
    });

    expect(governance.refresh).toHaveBeenCalledTimes(1);
    expect(screen.getByText('!!!Loading DRep data…')).toBeInTheDocument();
    expect(
      screen.queryByText(/was not found in the latest on-chain data/)
    ).not.toBeInTheDocument();
  });

  it('shows the inline not-found error with a working Back to directory link', () => {
    const { pushSpy } = renderPage({
      governanceOverrides: { drepIndex: new Map() },
      locationState: {
        from: ROUTES.VOTING.GOVERNANCE,
        selectedWalletId: 'wallet-1',
        voteType: 'drep',
      },
    });

    expect(
      screen.getByText(
        '!!!This DRep was not found in the latest on-chain data.'
      )
    ).toBeInTheDocument();

    fireEvent.click(screen.getByText('!!!Back to directory'));

    expect(pushSpy).toHaveBeenCalledWith(
      ROUTES.GOVERNANCE.DREPS,
      expect.objectContaining({
        from: ROUTES.VOTING.GOVERNANCE,
        selectedWalletId: 'wallet-1',
        voteType: 'drep',
      })
    );
  });

  it('forwards inherited state plus the byte-equal id on Select for delegation', () => {
    const { pushSpy } = renderPage({
      locationState: {
        from: ROUTES.VOTING.GOVERNANCE,
        selectedWalletId: 'wallet-1',
        voteType: 'drep',
      },
    });

    fireEvent.click(
      screen.getByRole('button', { name: '!!!Select for delegation' })
    );

    expect(pushSpy).toHaveBeenCalledWith(
      ROUTES.VOTING.GOVERNANCE,
      expect.objectContaining({
        from: ROUTES.VOTING.GOVERNANCE,
        selectedDRepId: DREP_ID,
        selectedWalletId: 'wallet-1',
        voteType: 'drep',
      })
    );
  });

  it('falls back to the governance form route when no state was inherited', () => {
    const { pushSpy } = renderPage();

    fireEvent.click(
      screen.getByRole('button', { name: '!!!Select for delegation' })
    );

    expect(pushSpy).toHaveBeenCalledWith(
      ROUTES.VOTING.GOVERNANCE,
      expect.objectContaining({ selectedDRepId: DREP_ID })
    );
  });

  it('refetches exactly once when the node reaches the tip', () => {
    const { governance, networkStatus } = renderPage({
      isNodeInSync: false,
      syncProgress: 99,
    });
    expect(governance.refresh).not.toHaveBeenCalled();

    act(() => {
      runInAction(() => {
        networkStatus.isNodeInSync = true;
        networkStatus.syncProgress = 100;
      });
    });

    expect(governance.refresh).toHaveBeenCalledTimes(1);
  });

  it('shows the copied confirmation after the copy button is clicked', async () => {
    const writeText = jest.fn(async () => undefined);
    Object.defineProperty(navigator, 'clipboard', {
      configurable: true,
      value: { writeText },
    });
    try {
      renderPage();

      fireEvent.click(
        screen.getByRole('button', { name: '!!!Copy CIP-129 DRep ID' })
      );

      expect(await screen.findByText('!!!DRep ID copied')).toBeInTheDocument();
      expect(writeText).toHaveBeenCalledWith(DREP_ID);
    } finally {
      delete (navigator as any).clipboard;
    }
  });

  it('renders both ID forms in full in the detail header', () => {
    const decodableEntry = { ...baseEntry, drepId: DECODABLE_DREP_ID };
    const { container } = renderPage({
      drepId: DECODABLE_DREP_ID,
      governanceOverrides: {
        drepIndex: new Map([[DECODABLE_DREP_ID, decodableEntry]]),
        drepList: [decodableEntry],
      },
    });

    expect(container.querySelectorAll('code')[0]).toHaveTextContent(
      DECODABLE_DREP_ID
    );
    expect(screen.getByText(DECODABLE_CIP105)).toBeInTheDocument();
    expect(screen.getByText('!!!(CIP-105)')).toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: '!!!Copy CIP-105 DRep ID' })
    ).toBeInTheDocument();
  });

  it('renders only the CIP-129 form when the id does not decode', () => {
    renderPage();

    expect(screen.queryByText('!!!(CIP-105)')).not.toBeInTheDocument();
    expect(
      screen.queryByRole('button', { name: '!!!Copy CIP-105 DRep ID' })
    ).not.toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: '!!!Copy CIP-129 DRep ID' })
    ).toBeInTheDocument();
  });

  it('renders the detail field labels in ja-JP', () => {
    renderPage({ locale: 'ja-JP' });

    expect(screen.getByText('!!!DRep詳細')).toBeInTheDocument();
    expect(screen.getByText('!!!ステータス')).toBeInTheDocument();
    expect(screen.getByText('!!!アンカー')).toBeInTheDocument();
    expect(screen.getByText('!!!34エポック')).toBeInTheDocument();
  });

  it('renders the category badge in the detail header (snapshot)', () => {
    renderPage();

    // baseEntry: verified, in cohort, at or below the median -> Primary.
    expect(
      screen.getByText('!!!Primary').closest('span[title]')
    ).toMatchSnapshot();
  });

  it('renders the high value badge when the entry is above the cohort median (snapshot)', () => {
    renderPage({
      governanceOverrides: {
        cohortContext: {
          medianVotingPower: new BigNumber('1000000'),
          memberIds: new Set([DREP_ID]),
          verifiedMetadataIds: new Set([DREP_ID]),
        },
      },
    });

    expect(
      screen.getByText('!!!High value').closest('span[title]')
    ).toMatchSnapshot();
  });

  it('requests the anchor content once on mount for an entry with an anchor', () => {
    const { governance } = renderPage();

    expect(governance.fetchAnchorContent).toHaveBeenCalledTimes(1);
    expect(governance.fetchAnchorContent).toHaveBeenCalledWith(
      DREP_ID,
      baseEntry.anchor
    );
  });

  it('requests no anchor content when the entry has no anchor', () => {
    const { governance } = renderPage({
      governanceOverrides: {
        drepIndex: new Map([[DREP_ID, { ...baseEntry, anchor: null }]]),
      },
    });

    expect(governance.fetchAnchorContent).not.toHaveBeenCalled();
  });

  it('renders the verified name with the verified off-chain label and host tooltip', () => {
    renderPage({
      governanceOverrides: { anchorStateByDRepId: verifiedState() },
    });

    expect(screen.getByText('Daedalus Test DRep')).toBeInTheDocument();
    const label = screen.getByText('!!!Verified off-chain content');
    expect(label).toBeInTheDocument();
    expect(label.getAttribute('title')).toEqual(
      expect.stringContaining('raw.githubusercontent.com')
    );
  });

  it('keeps every on-chain row when the anchor is unavailable', () => {
    renderPage({
      governanceOverrides: {
        anchorStateByDRepId: new Map([
          [
            DREP_ID,
            {
              state: 'unavailable',
              hash: baseEntry.anchor!.hash,
              reason: AnchorFetchErrorType.HttpStatus,
            },
          ],
        ]),
      },
    });

    expect(
      screen.getByText(
        '!!!The off-chain profile could not be verified. Only on-chain data is shown.'
      )
    ).toBeInTheDocument();
    expect(screen.getByText('!!!Status')).toBeInTheDocument();
    expect(screen.getByText('!!!Expires in')).toBeInTheDocument();
    expect(screen.getByText('!!!Voting power')).toBeInTheDocument();
    expect(screen.getByText('!!!Current votes')).toBeInTheDocument();
    expect(screen.getByText(baseEntry.anchor!.url)).toBeInTheDocument();
    expect(screen.getByText(baseEntry.anchor!.hash)).toBeInTheDocument();
    const referenceLabel = screen.getByText('!!!On-chain anchor reference');
    expect(referenceLabel).toBeInTheDocument();
    // The untooltipped variants must stay untooltipped, or the shared
    // CurrentVoteSummary snapshot drifts with them.
    expect(referenceLabel).not.toHaveAttribute('title');
  });

  it('renders the loading state without a name while the anchor is checked', () => {
    renderPage({
      governanceOverrides: {
        anchorStateByDRepId: new Map([
          [DREP_ID, { state: 'loading', hash: baseEntry.anchor!.hash }],
        ]),
      },
    });

    expect(screen.getByText('!!!Checking the anchor…')).toBeInTheDocument();
    expect(screen.queryByText('!!!Off-chain profile')).not.toBeInTheDocument();
    expect(screen.queryByText('Daedalus Test DRep')).not.toBeInTheDocument();
  });

  it('renders the verified block in ja-JP', () => {
    renderPage({
      locale: 'ja-JP',
      governanceOverrides: { anchorStateByDRepId: verifiedState() },
    });

    expect(screen.getByText('Daedalus Test DRep')).toBeInTheDocument();
    expect(
      screen.getByText('!!!検証済みオフチェーンコンテンツ')
    ).toBeInTheDocument();
    expect(screen.getByText('!!!オフチェーンプロフィール')).toBeInTheDocument();
  });

  it('renders every verified profile field with a verified off-chain label', () => {
    renderPage({
      governanceOverrides: {
        anchorStateByDRepId: verifiedState({
          objectives: 'Fixture objectives',
          motivations: 'Fixture motivations',
          qualifications: 'Fixture qualifications',
        }),
      },
    });

    expect(screen.getByText('!!!Objectives')).toBeInTheDocument();
    expect(screen.getByText('Fixture objectives')).toBeInTheDocument();
    expect(screen.getByText('!!!Motivations')).toBeInTheDocument();
    expect(screen.getByText('Fixture motivations')).toBeInTheDocument();
    expect(screen.getByText('!!!Qualifications')).toBeInTheDocument();
    expect(screen.getByText('Fixture qualifications')).toBeInTheDocument();
    expect(
      screen.getAllByText('!!!Verified off-chain content').length
    ).toBeGreaterThanOrEqual(4);
  });

  it('renders an identity reference under the claim caption and never as a plain link', () => {
    renderPage({
      governanceOverrides: {
        anchorStateByDRepId: verifiedState({
          references: [
            {
              type: 'link',
              label: 'Blog',
              uri: 'https://example.org/blog',
            },
            {
              type: 'identity',
              label: 'X profile',
              uri: 'https://example.org/id',
            },
          ],
        }),
      },
    });

    const identityHeading = screen.getByText('!!!Claimed identities');
    const linkHeading = screen.getByText('!!!Links');
    expect(identityHeading).toBeInTheDocument();
    expect(
      screen.getByText(
        '!!!These identities are claimed by the DRep and are not verified by Daedalus. Open the link and confirm that this DRep ID is published there before you rely on it.'
      )
    ).toBeInTheDocument();

    // The identity entry must sit after the caption, never inside the Links list.
    const identityEntry = screen.getByText('X profile');
    const blogEntry = screen.getByText('Blog');
    expect(identityHeading.compareDocumentPosition(identityEntry)).toBe(
      Node.DOCUMENT_POSITION_FOLLOWING
    );
    expect(linkHeading.compareDocumentPosition(blogEntry)).toBe(
      Node.DOCUMENT_POSITION_FOLLOWING
    );
    expect(identityEntry.closest('ul')).not.toBe(blogEntry.closest('ul'));
  });

  it('buckets an unrecognised reference type under other references', () => {
    renderPage({
      governanceOverrides: {
        anchorStateByDRepId: verifiedState({
          references: [
            { type: 'other', label: null, uri: 'https://example.org/misc' },
          ],
        }),
      },
    });

    expect(screen.getByText('!!!Other references')).toBeInTheDocument();
    expect(screen.getByText('https://example.org/misc')).toBeInTheDocument();
    expect(screen.queryByText('!!!Claimed identities')).not.toBeInTheDocument();
  });

  it('opens an https reference uri through the external-link handler', () => {
    const { app } = renderPage({
      governanceOverrides: {
        anchorStateByDRepId: verifiedState({
          references: [
            { type: 'link', label: 'Blog', uri: 'https://example.org/blog' },
          ],
        }),
      },
    });

    const link = screen.getByText('Blog').closest('a');
    expect(link).toHaveAttribute('href', 'https://example.org/blog');
    expect(link).toHaveAttribute('rel', 'noopener noreferrer');

    fireEvent.click(link!);

    expect(app.openExternalLink).toHaveBeenCalledWith(
      'https://example.org/blog'
    );
  });

  it('renders a non-https reference uri as inert text', () => {
    const { app } = renderPage({
      governanceOverrides: {
        anchorStateByDRepId: verifiedState({
          references: [
            { type: 'link', label: null, uri: 'http://example.org/plain' },
          ],
        }),
      },
    });

    const entry = screen.getByText('http://example.org/plain');
    expect(entry.closest('a')).toBeNull();
    expect(app.openExternalLink).not.toHaveBeenCalled();
  });

  it('renders the stated payment address read-only with a working copy button', async () => {
    const address = 'addr1qxexamplepaymentaddressvalue';
    const writeText = jest.fn(async () => undefined);
    Object.defineProperty(navigator, 'clipboard', {
      configurable: true,
      value: { writeText },
    });
    try {
      renderPage({
        governanceOverrides: {
          anchorStateByDRepId: verifiedState({ paymentAddress: address }),
        },
      });

      expect(screen.getByText('!!!Stated payment address')).toBeInTheDocument();
      expect(
        screen.getByText(
          "!!!This address is the DRep's own claim. Delegating your voting power requires no payment to any address."
        )
      ).toBeInTheDocument();
      expect(screen.getByText(address).tagName).toBe('SPAN');
      expect(screen.queryByDisplayValue(address)).not.toBeInTheDocument();

      fireEvent.click(
        screen.getByRole('button', { name: '!!!Copy stated payment address' })
      );

      expect(writeText).toHaveBeenCalledWith(address);
      expect(
        await screen.findByText('!!!Payment address copied')
      ).toBeInTheDocument();
    } finally {
      delete (navigator as any).clipboard;
    }
  });

  it('reaches no logger on either payment-address copy path', async () => {
    const address = 'addr1qxexamplepaymentaddressvalue';
    const spies = (['debug', 'info', 'warn', 'error'] as const).map((level) =>
      jest.spyOn(rendererLogger, level).mockImplementation(() => undefined)
    );
    const writeText = jest.fn(async () => undefined);
    Object.defineProperty(navigator, 'clipboard', {
      configurable: true,
      value: { writeText },
    });
    try {
      renderPage({
        governanceOverrides: {
          anchorStateByDRepId: verifiedState({ paymentAddress: address }),
        },
      });
      const copyButton = () =>
        screen.getByRole('button', { name: '!!!Copy stated payment address' });

      fireEvent.click(copyButton());
      await screen.findByText('!!!Payment address copied');

      // The unavailable branch must be as silent as the success branch: no
      // length, no error code, nothing that could carry the address.
      delete (navigator as any).clipboard;
      fireEvent.click(copyButton());

      spies.forEach((spy) => expect(spy).not.toHaveBeenCalled());
    } finally {
      delete (navigator as any).clipboard;
      spies.forEach((spy) => spy.mockRestore());
    }
  });

  it('renders the profile block when references and payment address are absent', () => {
    renderPage({
      governanceOverrides: {
        anchorStateByDRepId: verifiedState({ objectives: 'Only objectives' }),
      },
    });

    expect(screen.getByText('!!!Off-chain profile')).toBeInTheDocument();
    expect(screen.getByText('Only objectives')).toBeInTheDocument();
    expect(screen.queryByText('!!!References')).not.toBeInTheDocument();
    expect(
      screen.queryByText('!!!Stated payment address')
    ).not.toBeInTheDocument();
  });

  it('renders the profile block with no name when the anchor carries only prose', () => {
    renderPage({
      governanceOverrides: {
        anchorStateByDRepId: verifiedState({
          givenName: null,
          objectives: 'Objectives only',
        }),
      },
    });

    expect(screen.getByText('Objectives only')).toBeInTheDocument();
    expect(screen.queryByText('!!!Name')).not.toBeInTheDocument();
    // The name caption is name-specific copy and must not appear without a name.
    expect(
      screen.queryByText(
        "!!!This name is the DRep's own claim, hash-matched to the anchor recorded on-chain. Daedalus does not verify identity."
      )
    ).not.toBeInTheDocument();
  });

  it('renders the new profile labels in ja-JP', () => {
    renderPage({
      locale: 'ja-JP',
      governanceOverrides: {
        anchorStateByDRepId: verifiedState({
          objectives: 'Fixture objectives',
          paymentAddress: 'addr1qxexamplepaymentaddressvalue',
          references: [
            {
              type: 'identity',
              label: 'X profile',
              uri: 'https://example.org/id',
            },
          ],
        }),
      },
    });

    expect(screen.getByText('!!!目的')).toBeInTheDocument();
    expect(
      screen.getByText('!!!申告されたアイデンティティ')
    ).toBeInTheDocument();
    expect(screen.getByText('!!!申告された支払いアドレス')).toBeInTheDocument();
  });

  it('opens an https anchor url through the external-link handler', () => {
    const { app } = renderPage();

    const link = screen.getByText(baseEntry.anchor!.url).closest('a');
    expect(link).not.toBeNull();
    expect(link).toHaveAttribute('href', baseEntry.anchor!.url);
    expect(link).toHaveAttribute('target', '_blank');
    expect(link).toHaveAttribute('rel', 'noopener noreferrer');

    const defaultNotPrevented = fireEvent.click(link!);

    expect(defaultNotPrevented).toBe(false);
    expect(app.openExternalLink).toHaveBeenCalledTimes(1);
    expect(app.openExternalLink).toHaveBeenCalledWith(baseEntry.anchor!.url);
  });

  it('renders a non-https anchor url as inert text', () => {
    const httpUrl = 'http://anchor.example.org/profile.jsonld';
    const { app } = renderPage({
      governanceOverrides: {
        drepIndex: new Map([
          [
            DREP_ID,
            { ...baseEntry, anchor: { ...baseEntry.anchor!, url: httpUrl } },
          ],
        ]),
      },
    });

    const urlText = screen.getByText(httpUrl);
    expect(urlText).toBeInTheDocument();
    expect(urlText.closest('a')).toBeNull();
    expect(app.openExternalLink).not.toHaveBeenCalled();
  });
});
