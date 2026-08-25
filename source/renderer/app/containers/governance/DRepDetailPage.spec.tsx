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
import { logger as rendererLogger } from '../../utils/logging';
import translations from '../../i18n/locales/en-US.json';
import jaTranslations from '../../i18n/locales/ja-JP.json';
import { ROUTES } from '../../routes-config';
import { GovernanceRefreshState } from '../../stores/GovernanceStore';
import type {
  AppDRepDirectoryEntry,
  AppDRepDetail,
} from '../../stores/GovernanceStore';
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

const baseDetail: AppDRepDetail = {
  ...baseEntry,
  metadata: null,
};

const makeMetadata = (
  overrides: Partial<NonNullable<AppDRepDetail['metadata']>> = {}
): NonNullable<AppDRepDetail['metadata']> => ({
  objectives: null,
  motivations: null,
  qualifications: null,
  paymentAddress: null,
  references: [],
  additionalFields: [],
  ...overrides,
});

const buildGovernanceStore = (overrides: Record<string, unknown> = {}) => {
  const store = {
    error: null,
    favoriteDRepIds: new Set<string>(),
    fetchDRep: jest.fn().mockResolvedValue(baseDetail),
    lastFetchedAt: Date.now() - 60_000,
    refresh: jest.fn(),
    refreshState: GovernanceRefreshState.Loaded,
    toggleFavorite: jest.fn(),
    delegationNavState: null as Record<string, unknown> | null,
    setDelegationNavState: jest.fn(),
    ...overrides,
  };
  // Wire setDelegationNavState to mutate delegationNavState in place.
  if (!overrides.setDelegationNavState) {
    store.setDelegationNavState = jest.fn(
      (state: Record<string, unknown> | null) => {
        store.delegationNavState = state;
      }
    );
  }
  return store;
};

const renderPage = async ({
  governanceOverrides = {},
  drepId = DREP_ID,
  isNodeInSync = true,
  locale = 'en-US',
  delegationNavState,
  syncProgress = 100,
}: {
  governanceOverrides?: Record<string, unknown>;
  drepId?: string;
  isNodeInSync?: boolean;
  locale?: string;
  delegationNavState?: Record<string, unknown>;
  syncProgress?: number | null;
} = {}) => {
  // Observable so the container's reaction sees the flip like the real store.
  const networkStatus = observable({ isNodeInSync, syncProgress });
  const governance = buildGovernanceStore({
    ...governanceOverrides,
    ...(delegationNavState != null ? { delegationNavState } : {}),
  });
  // The page compares a published payment address against the running network,
  // so the harness has to have one.
  const app = {
    openExternalLink: jest.fn(),
    environment: { network: 'mainnet' },
  };
  const history = createMemoryHistory({
    initialEntries: [{ pathname: `${ROUTES.GOVERNANCE.DREPS}/${drepId}` }],
  });
  const pushSpy = jest.spyOn(history, 'push');
  const messages = locale === 'ja-JP' ? jaTranslations : translations;
  let view: ReturnType<typeof render>;
  await act(async () => {
    view = render(
      <Provider
        stores={
          {
            app,
            governance,
            networkStatus,
            wallets: {
              all: [{ id: 'wallet-1' }],
              allWallets: [{ id: 'wallet-1' }],
            },
          } as any
        }
      >
        <IntlProvider locale={locale} messages={messages}>
          <Router history={history}>
            <Route path={DETAIL_PATH} component={DRepDetailPage} />
          </Router>
        </IntlProvider>
      </Provider>
    );
  });
  return { app, governance, history, networkStatus, pushSpy, ...view! };
};

describe('DRepDetailPage', () => {
  afterEach(() => {
    cleanup();
    jest.restoreAllMocks();
  });

  it('renders the on-chain fields for a loaded entry', async () => {
    await renderPage();

    // No generic page title: the tab bar and the back link already place us.
    expect(screen.queryByText('!!!DRep detail')).toBeNull();
    expect(screen.getByText('!!!Active')).toBeInTheDocument();
    expect(screen.getByText('!!!34 epochs')).toBeInTheDocument();
    // The full ADA figure is exact and unabbreviated here, so restating it in
    // lovelace said the same thing twice.
    expect(screen.getByText('₳ 23,137,980.123456')).toBeInTheDocument();
    expect(screen.queryByText(/lovelace\)/)).toBeNull();
  });

  it('names each section for where its contents came from', async () => {
    await renderPage();

    // The two boxes already separate what the ledger reports from what the
    // DRep published, so each is named for that rather than repeating a small
    // provenance label beside individual fields.
    expect(screen.getByText('!!!On-Chain Data')).toBeInTheDocument();
    expect(screen.getByText('!!!Off-Chain Metadata')).toBeInTheDocument();
    expect(
      screen.getByLabelText(
        '!!!Read directly from the Cardano ledger by your local node.'
      )
    ).toBeInTheDocument();
    // The redundant per-row label inside the on-chain box is gone; the anchor
    // box keeps its own, which distinguishes the on-chain pointer from the
    // off-chain content behind it.
    // No provenance labels inside either box: naming the box says it once.
    expect(screen.queryByText('!!!Source')).toBeNull();
    expect(screen.queryByText('!!!Verified off-chain content')).toBeNull();
  });

  it('keeps the on-chain section complete and labelled with no anchor data', async () => {
    await renderPage({
      governanceOverrides: {
        fetchDRep: jest.fn().mockResolvedValue({ ...baseDetail, anchor: null }),
      },
    });

    expect(screen.getByText('Status')).toBeInTheDocument();
    expect(screen.getByText('!!!Inactive in')).toBeInTheDocument();
    expect(screen.getByText('!!!Voting power')).toBeInTheDocument();
    expect(screen.getByText('!!!On-Chain Data')).toBeInTheDocument();
    expect(screen.queryByText('!!!Verified off-chain content')).toBeNull();
  });

  it('flags an anchor whose contents Daedalus could not verify', async () => {
    // baseDetail registers an anchor but carries no metadata, which is what
    // the wallet returns when the fetch or the hash check fails.
    await renderPage();

    expect(screen.getByText('!!!Unverified')).toBeInTheDocument();
    // The blurb has to say what happened, not merely that something did:
    // Daedalus could not match the document to the on-chain hash, so nothing
    // from it is shown, the link is still offered, and what is behind it is
    // the DRep's own claim.
    expect(
      screen.getByText(/could not match the document at this anchor/)
    ).toBeInTheDocument();
    expect(screen.getByText(/this DRep's own claim/)).toBeInTheDocument();
  });

  it('separates a verified but empty document from one that did not verify', async () => {
    await renderPage({
      governanceOverrides: {
        fetchDRep: jest.fn().mockResolvedValue({
          ...baseDetail,
          verifiedName: null,
          metadata: makeMetadata(),
        }),
      },
    });

    // Two different facts. This one verified, so nothing is being withheld and
    // nothing is wrong: the document holds no fields of any kind.
    expect(screen.getByText('!!!Nothing published')).toBeInTheDocument();
    expect(screen.queryByText('!!!Unverified')).not.toBeInTheDocument();
    expect(
      screen.getByText(/verified this document against its on-chain hash/)
    ).toBeInTheDocument();
  });

  it('shows a document that carries only fields no standard defines', async () => {
    await renderPage({
      governanceOverrides: {
        fetchDRep: jest.fn().mockResolvedValue({
          ...baseDetail,
          verifiedName: null,
          metadata: makeMetadata({
            additionalFields: [
              { key: 'twitter', value: { kind: 'text', text: '@example' } },
            ],
          }),
        }),
      },
    });

    // Every canonical field is absent, but the document is not empty: it
    // carries a field under the author's own key. Calling that nothing
    // published would both misstate it and hide the one thing it holds.
    expect(screen.queryByText('!!!Nothing published')).not.toBeInTheDocument();
    expect(screen.getByText('twitter')).toBeInTheDocument();
    expect(screen.getByText('@example')).toBeInTheDocument();
    // And the heading for the standardised fields stays away, since there
    // are none of those to head.
    expect(
      screen.queryByText('!!!Canonical metadata fields')
    ).not.toBeInTheDocument();
  });

  it('warns when a published payment address is for another network', async () => {
    await renderPage({
      governanceOverrides: {
        fetchDRep: jest.fn().mockResolvedValue({
          ...baseDetail,
          verifiedName: 'Daedalus Test DRep',
          metadata: makeMetadata({
            paymentAddress:
              'addr_test1qz2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3n0d3vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgsrgxksj',
          }),
        }),
      },
    });

    // The prefix is all it takes. Silence here leaves someone to copy the
    // address and find out by sending.
    expect(
      screen.getByText(/Nothing sent from this wallet can reach it/)
    ).toBeInTheDocument();
  });

  it('says nothing about a payment address that matches the network', async () => {
    await renderPage({
      governanceOverrides: {
        fetchDRep: jest.fn().mockResolvedValue({
          ...baseDetail,
          verifiedName: 'Daedalus Test DRep',
          metadata: makeMetadata({
            paymentAddress:
              'addr1qyr53s0h929lksqp5v8rhlveu4skwp8ugdz87ghaswu95v6q9mncexq3sz7phzf5x4yuez5ljkhfauj6puptdtp86ekq8ndej2',
          }),
        }),
      },
    });

    expect(
      screen.queryByText(/Nothing sent from this wallet can reach it/)
    ).not.toBeInTheDocument();
  });

  it('opens no canonical block for a document carrying only a name', async () => {
    await renderPage({
      governanceOverrides: {
        fetchDRep: jest.fn().mockResolvedValue({
          ...baseDetail,
          verifiedName: 'Named But Otherwise Empty',
          metadata: makeMetadata(),
        }),
      },
    });

    // The name is the page heading, not a row in this block, so a heading with
    // nothing under it is what counting the name here produced.
    expect(screen.getByText('Named But Otherwise Empty')).toBeInTheDocument();
    expect(
      screen.queryByText('!!!Canonical metadata fields')
    ).not.toBeInTheDocument();
  });

  it('closes the canonical block before opening the additional one', async () => {
    await renderPage({
      governanceOverrides: {
        fetchDRep: jest.fn().mockResolvedValue({
          ...baseDetail,
          verifiedName: 'Daedalus Test DRep',
          metadata: makeMetadata({
            references: [
              { type: 'link', label: 'Blog', uri: 'https://example.org/blog' },
            ],
            additionalFields: [
              {
                key: 'nationality',
                value: { kind: 'text' as const, text: 'Japan' },
              },
            ],
          }),
        }),
      },
    });

    // References is a field CIP-119 defines. With the additional heading above
    // it, it read as something the DRep invented.
    const headings = Array.from(document.querySelectorAll('h3, h4')).map(
      (node) => node.textContent?.trim()
    );
    const referencesAt = headings.findIndex((text) =>
      text?.includes('!!!References')
    );
    const additionalAt = headings.findIndex((text) =>
      text?.includes('!!!Additional metadata fields')
    );

    expect(referencesAt).toBeGreaterThan(-1);
    expect(additionalAt).toBeGreaterThan(-1);
    expect(referencesAt).toBeLessThan(additionalAt);
  });

  it('says nothing about verification when there is no anchor at all', async () => {
    await renderPage({
      governanceOverrides: {
        fetchDRep: jest.fn().mockResolvedValue({ ...baseDetail, anchor: null }),
      },
    });

    // Nothing was claimed, so there is nothing to have failed to verify.
    expect(screen.queryByText('!!!Unverified')).not.toBeInTheDocument();
  });

  it('renders the anchor url and hash under the off-chain heading', async () => {
    await renderPage();

    expect(
      screen.getByText('https://governance-preview.example.org/dreps/1.json')
    ).toBeInTheDocument();
    expect(screen.getByText(baseEntry.anchor!.hash)).toBeInTheDocument();
    // The heading carries the provenance; the row does not repeat it.
    expect(screen.getByText('!!!Off-Chain Metadata')).toBeInTheDocument();
    expect(screen.queryByText('!!!On-chain anchor reference')).toBeNull();
  });

  it('renders the anchor-absent message when no anchor is recorded', async () => {
    await renderPage({
      governanceOverrides: {
        fetchDRep: jest.fn().mockResolvedValue({ ...baseDetail, anchor: null }),
      },
    });

    expect(
      screen.getByText('!!!This DRep submitted no off-chain metadata record.')
    ).toBeInTheDocument();
    expect(
      screen.queryByText('!!!On-chain anchor reference')
    ).not.toBeInTheDocument();
  });

  it('says why the voting power is missing when stake enrichment failed', async () => {
    await renderPage({
      governanceOverrides: {
        fetchDRep: jest
          .fn()
          .mockResolvedValue({ ...baseDetail, votingPower: null }),
      },
    });

    // The voting power row states the reason as text rather than hiding it
    // in a tooltip on a dash. The share row below it, which needs that same
    // figure, is left as a bare dash so the reason is given once.
    expect(
      screen.getByText('!!!Stake distribution unavailable, try again later.')
    ).toBeInTheDocument();
    expect(screen.getAllByText('—')).toHaveLength(1);
  });

  it('shows the loading state until fetchDRep resolves', async () => {
    // Never resolves — component stays in Loading state.
    const neverResolves = jest.fn().mockReturnValue(new Promise(() => {}));
    let view: ReturnType<typeof render>;
    act(() => {
      view = render(
        <Provider
          stores={
            {
              app: {
                openExternalLink: jest.fn(),
                environment: { network: 'mainnet' },
              },
              governance: buildGovernanceStore({ fetchDRep: neverResolves }),
              networkStatus: observable({
                isNodeInSync: true,
                syncProgress: 100,
              }),
            } as any
          }
        >
          <IntlProvider locale="en-US" messages={translations}>
            <Router
              history={createMemoryHistory({
                initialEntries: [
                  { pathname: `${ROUTES.GOVERNANCE.DREPS}/${DREP_ID}` },
                ],
              })}
            >
              <Route path={DETAIL_PATH} component={DRepDetailPage} />
            </Router>
          </IntlProvider>
        </Provider>
      );
    });

    expect(screen.getByText('!!!Loading DRep data…')).toBeInTheDocument();
    expect(
      screen.queryByText(/was not found in the on-chain data/)
    ).not.toBeInTheDocument();
  });

  it('refreshes on mount from an empty Idle store', async () => {
    const { governance } = await renderPage({
      governanceOverrides: {
        refreshState: GovernanceRefreshState.Idle,
      },
    });

    expect(governance.refresh).toHaveBeenCalledTimes(1);
  });

  it('shows the inline not-found error with a working Back to directory link', async () => {
    const { pushSpy } = await renderPage({
      governanceOverrides: {
        fetchDRep: jest.fn().mockRejectedValue(new Error('not found')),
      },
      delegationNavState: {
        from: ROUTES.VOTING.GOVERNANCE,
        selectedWalletId: 'wallet-1',
        voteType: 'drep',
      },
    });

    expect(
      screen.getByText('!!!This DRep was not found in the on-chain data.')
    ).toBeInTheDocument();

    fireEvent.click(screen.getByText('!!!Back to directory'));

    expect(pushSpy).toHaveBeenCalledWith(ROUTES.GOVERNANCE.DREPS);
  });

  it('sets delegationNavState with inherited context + byte-equal id on Select for delegation', async () => {
    const { pushSpy, governance } = await renderPage({
      delegationNavState: {
        from: ROUTES.VOTING.GOVERNANCE,
        selectedWalletId: 'wallet-1',
        voteType: 'drep',
      },
    });

    fireEvent.click(screen.getByRole('button', { name: 'Delegate' }));

    expect(governance.setDelegationNavState).toHaveBeenCalledWith(
      expect.objectContaining({
        from: ROUTES.VOTING.GOVERNANCE,
        selectedDRepId: DREP_ID,
        selectedWalletId: 'wallet-1',
        voteType: 'drep',
      })
    );
    expect(pushSpy).toHaveBeenCalledWith(ROUTES.VOTING.GOVERNANCE);
  });

  it('falls back to the governance form route when no delegationNavState was inherited', async () => {
    const { pushSpy, governance } = await renderPage();

    fireEvent.click(screen.getByRole('button', { name: 'Delegate' }));

    expect(governance.setDelegationNavState).toHaveBeenCalledWith(
      expect.objectContaining({ selectedDRepId: DREP_ID })
    );
    expect(pushSpy).toHaveBeenCalledWith(ROUTES.VOTING.GOVERNANCE);
  });

  it('refetches exactly once when the node reaches the tip', async () => {
    const { governance, networkStatus } = await renderPage({
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
      await renderPage();

      fireEvent.click(
        screen.getByRole('button', { name: '!!!Copy CIP-129 DRep ID' })
      );

      expect(await screen.findByText('!!!DRep ID copied')).toBeInTheDocument();
      expect(writeText).toHaveBeenCalledWith(DREP_ID);
    } finally {
      delete (navigator as any).clipboard;
    }
  });

  it('renders both ID forms in full in the detail header', async () => {
    const decodableDetail = { ...baseDetail, drepId: DECODABLE_DREP_ID };
    const { container } = await renderPage({
      drepId: DECODABLE_DREP_ID,
      governanceOverrides: {
        fetchDRep: jest.fn().mockResolvedValue(decodableDetail),
      },
    });

    expect(container.querySelectorAll('code')[0]).toHaveTextContent(
      DECODABLE_DREP_ID
    );
    // CIP-105 is deprecated and sits behind a disclosure now.
    expect(screen.queryByText(DECODABLE_CIP105)).toBeNull();
    fireEvent.click(screen.getByText('!!!Show deprecated CIP-105 ID'));
    expect(screen.getByText(DECODABLE_CIP105)).toBeInTheDocument();
    expect(screen.getByText('!!!(CIP-105)')).toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: '!!!Copy CIP-105 DRep ID' })
    ).toBeInTheDocument();
  });

  it('renders only the CIP-129 form when the id does not decode', async () => {
    await renderPage();

    expect(screen.queryByText('!!!(CIP-105)')).not.toBeInTheDocument();
    expect(
      screen.queryByRole('button', { name: '!!!Copy CIP-105 DRep ID' })
    ).not.toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: '!!!Copy CIP-129 DRep ID' })
    ).toBeInTheDocument();
  });

  it('renders the detail field labels in ja-JP', async () => {
    await renderPage({ locale: 'ja-JP' });

    expect(screen.queryByText('!!!DRep詳細')).toBeNull();
    expect(screen.getByText('ステータス')).toBeInTheDocument();
    expect(screen.getByText('!!!オフチェーンメタデータ')).toBeInTheDocument();
    expect(screen.getByText('!!!34エポック')).toBeInTheDocument();
  });

  it('carries no metadata badge, the sections having already said it', async () => {
    await renderPage();

    // A DRep either has an off-chain metadata section or it does not, which
    // says the same thing the badge said, at greater length and in two places.
    expect(screen.queryByText('!!!No metadata')).toBeNull();
    expect(screen.queryByText('!!!Verified')).toBeNull();
    expect(screen.queryByText('!!!Inactive Soon')).toBeNull();
    expect(screen.getByText('!!!Active')).toBeInTheDocument();
  });

  it('renders the verified name with the off-chain label and anchor host tooltip', async () => {
    await renderPage({
      governanceOverrides: {
        fetchDRep: jest.fn().mockResolvedValue({
          ...baseDetail,
          verifiedName: 'Daedalus Test DRep',
          metadata: makeMetadata(),
        }),
      },
    });

    // The name identifies the DRep, so it sits at the top beside the id and
    // titles the page in place of a generic heading.
    const name = screen.getByText('Daedalus Test DRep');
    expect(name).toBeInTheDocument();
    expect(name.tagName).toBe('H1');
    expect(screen.queryByText('!!!Verified off-chain content')).toBeNull();
  });

  it('renders the verified block in ja-JP', async () => {
    await renderPage({
      locale: 'ja-JP',
      governanceOverrides: {
        fetchDRep: jest.fn().mockResolvedValue({
          ...baseDetail,
          verifiedName: 'Daedalus Test DRep',
          // Content, not just a name: the block exists to hold fields, and a
          // name alone is the page heading rather than a row in it.
          metadata: makeMetadata({ objectives: '説明' }),
        }),
      },
    });

    expect(screen.getByText('Daedalus Test DRep')).toBeInTheDocument();
    expect(screen.getByText('!!!オフチェーンメタデータ')).toBeInTheDocument();
    expect(screen.getByText('!!!標準メタデータ項目')).toBeInTheDocument();
  });

  it('renders every verified profile field under the one heading', async () => {
    await renderPage({
      governanceOverrides: {
        fetchDRep: jest.fn().mockResolvedValue({
          ...baseDetail,
          verifiedName: 'Daedalus Test DRep',
          metadata: makeMetadata({
            objectives: 'Fixture objectives',
            motivations: 'Fixture motivations',
            qualifications: 'Fixture qualifications',
          }),
        }),
      },
    });

    expect(screen.getByText('!!!Objectives')).toBeInTheDocument();
    expect(screen.getByText('Fixture objectives')).toBeInTheDocument();
    expect(screen.getByText('!!!Motivations')).toBeInTheDocument();
    expect(screen.getByText('Fixture motivations')).toBeInTheDocument();
    expect(screen.getByText('!!!Qualifications')).toBeInTheDocument();
    expect(screen.getByText('Fixture qualifications')).toBeInTheDocument();
    // One heading names the provenance for the whole box; the fields no longer
    // each carry a label saying the same thing.
    expect(screen.getByText('!!!Off-Chain Metadata')).toBeInTheDocument();
    expect(screen.queryByText('!!!Verified off-chain content')).toBeNull();
  });

  it('renders an identity reference under its heading and never as a plain link', async () => {
    await renderPage({
      governanceOverrides: {
        fetchDRep: jest.fn().mockResolvedValue({
          ...baseDetail,
          verifiedName: 'Daedalus Test DRep',
          metadata: makeMetadata({
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
        }),
      },
    });

    const identityHeading = screen.getByText('!!!Claimed identities');
    const linkHeading = screen.getByText('!!!Links');
    expect(identityHeading).toBeInTheDocument();
    // The caveat is available on the heading rather than set between the
    // heading and the entries it qualifies.
    expect(
      screen.getByLabelText(
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

  it('renders an unrecognised reference type as a link in the Links section', async () => {
    await renderPage({
      governanceOverrides: {
        fetchDRep: jest.fn().mockResolvedValue({
          ...baseDetail,
          verifiedName: 'Daedalus Test DRep',
          metadata: makeMetadata({
            references: [
              { type: 'other', label: null, uri: 'https://example.org/misc' },
            ],
          }),
        }),
      },
    });

    expect(screen.getByText('!!!Links')).toBeInTheDocument();
    expect(screen.getByText('https://example.org/misc')).toBeInTheDocument();
    expect(screen.queryByText('!!!Other references')).not.toBeInTheDocument();
    expect(screen.queryByText('!!!Claimed identities')).not.toBeInTheDocument();
  });

  it('opens an https reference uri through the external-link handler', async () => {
    const { app } = await renderPage({
      governanceOverrides: {
        fetchDRep: jest.fn().mockResolvedValue({
          ...baseDetail,
          verifiedName: 'Daedalus Test DRep',
          metadata: makeMetadata({
            references: [
              { type: 'link', label: 'Blog', uri: 'https://example.org/blog' },
            ],
          }),
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

  it('renders a non-https reference uri as inert text', async () => {
    const { app } = await renderPage({
      governanceOverrides: {
        fetchDRep: jest.fn().mockResolvedValue({
          ...baseDetail,
          verifiedName: 'Daedalus Test DRep',
          metadata: makeMetadata({
            references: [
              { type: 'link', label: null, uri: 'http://example.org/plain' },
            ],
          }),
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
      await renderPage({
        governanceOverrides: {
          fetchDRep: jest.fn().mockResolvedValue({
            ...baseDetail,
            verifiedName: 'Daedalus Test DRep',
            metadata: makeMetadata({ paymentAddress: address }),
          }),
        },
      });

      expect(screen.getByText('!!!Stated payment address')).toBeInTheDocument();
      // The claim caveat is on the heading's info control rather than set
      // between the heading and the address.
      expect(
        screen.getByLabelText(
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
      await renderPage({
        governanceOverrides: {
          fetchDRep: jest.fn().mockResolvedValue({
            ...baseDetail,
            verifiedName: 'Daedalus Test DRep',
            metadata: makeMetadata({ paymentAddress: address }),
          }),
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

  it('renders the profile block when references and payment address are absent', async () => {
    await renderPage({
      governanceOverrides: {
        fetchDRep: jest.fn().mockResolvedValue({
          ...baseDetail,
          verifiedName: 'Daedalus Test DRep',
          metadata: makeMetadata({ objectives: 'Only objectives' }),
        }),
      },
    });

    // Named for the standard, so the block for fields outside it can be named
    // for that and a reader can tell which labels are ours.
    expect(
      screen.getByText('!!!Canonical metadata fields')
    ).toBeInTheDocument();
    expect(screen.getByText('Only objectives')).toBeInTheDocument();
    expect(screen.queryByText('!!!References')).not.toBeInTheDocument();
    expect(
      screen.queryByText('!!!Stated payment address')
    ).not.toBeInTheDocument();
  });

  it('renders the profile block with no name when the entry carries only prose', async () => {
    await renderPage({
      governanceOverrides: {
        fetchDRep: jest.fn().mockResolvedValue({
          ...baseDetail,
          verifiedName: null,
          metadata: makeMetadata({ objectives: 'Objectives only' }),
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

  it('renders the new profile labels in ja-JP', async () => {
    await renderPage({
      locale: 'ja-JP',
      governanceOverrides: {
        fetchDRep: jest.fn().mockResolvedValue({
          ...baseDetail,
          verifiedName: 'Daedalus Test DRep',
          metadata: makeMetadata({
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
        }),
      },
    });

    expect(screen.getByText('!!!目的')).toBeInTheDocument();
    expect(
      screen.getByText('!!!申告されたアイデンティティ')
    ).toBeInTheDocument();
    expect(screen.getByText('!!!申告された支払いアドレス')).toBeInTheDocument();
  });

  it('opens an https anchor url through the external-link handler', async () => {
    const { app } = await renderPage();

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

  it('renders a non-https anchor url as inert text', async () => {
    const httpUrl = 'http://anchor.example.org/profile.jsonld';
    const { app } = await renderPage({
      governanceOverrides: {
        fetchDRep: jest.fn().mockResolvedValue({
          ...baseDetail,
          anchor: { ...baseDetail.anchor!, url: httpUrl },
        }),
      },
    });

    const urlText = screen.getByText(httpUrl);
    expect(urlText).toBeInTheDocument();
    expect(urlText.closest('a')).toBeNull();
    expect(app.openExternalLink).not.toHaveBeenCalled();
  });

  it('calls toggleFavorite on Select for delegation when the DRep is not already favorited', async () => {
    const { governance } = await renderPage();

    fireEvent.click(screen.getByRole('button', { name: 'Delegate' }));

    expect(governance.toggleFavorite).toHaveBeenCalledTimes(1);
    expect(governance.toggleFavorite).toHaveBeenCalledWith(DREP_ID);
  });

  it('does not call toggleFavorite on Select for delegation when already favorited', async () => {
    const { governance } = await renderPage({
      governanceOverrides: { favoriteDRepIds: new Set([DREP_ID]) },
    });

    fireEvent.click(screen.getByRole('button', { name: 'Delegate' }));

    expect(governance.toggleFavorite).not.toHaveBeenCalled();
  });

  it('renders the favorite toggle as unpressed (☆) when the entry is not favorited', async () => {
    await renderPage();

    const toggle = screen.getByRole('button', { name: /Add to favorites/ });
    expect(toggle).toHaveAttribute('aria-pressed', 'false');
    expect(toggle).toHaveTextContent('☆');
  });

  it('renders the favorite toggle as pressed (★) when the entry is already favorited', async () => {
    await renderPage({
      governanceOverrides: { favoriteDRepIds: new Set([DREP_ID]) },
    });

    const toggle = screen.getByRole('button', {
      name: /Remove from favorites/,
    });
    expect(toggle).toHaveAttribute('aria-pressed', 'true');
    expect(toggle).toHaveTextContent('★');
  });

  it('calls toggleFavorite with the DRep id when the favorite toggle button is clicked', async () => {
    const { governance } = await renderPage();

    fireEvent.click(screen.getByRole('button', { name: /Add to favorites/ }));

    expect(governance.toggleFavorite).toHaveBeenCalledTimes(1);
    expect(governance.toggleFavorite).toHaveBeenCalledWith(DREP_ID);
  });
});
