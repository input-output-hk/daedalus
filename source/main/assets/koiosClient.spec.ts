/**
 * The pointer client, against a stubbed transport.
 *
 * Three properties carry the weight here and none of them is "it parses a
 * response": that a batch costs two requests and not two per asset, that the
 * request body carries subjects and nothing else, and that every refusal leaves
 * rows absent rather than surfacing anywhere.
 */
import {
  KOIOS_ASSET_INFO_FIELDS,
  KOIOS_MAX_SUBJECTS_PER_BATCH,
  KOIOS_THROTTLED_RETRY_MS,
  KoiosRequestBudget,
  koiosBackoffMs,
  koiosBatches,
  koiosEndpoint,
  queryKoiosPointers,
} from './koiosClient';
import type { HttpTransport, HttpTransportResult } from './httpTransport';

const BASE = 'https://preprod.koios.rest/api/v1';
const POLICY = '67ab0c92c4ac1610895a1c965ee50aba41a8f1513b15240723b3bd0b';
const ASSET_NAME =
  '10b5e99cd9a171db19a101e9bb4afcb3b449a0aa504fe05eed13708bf3000001';
const SUBJECT = `${POLICY}${ASSET_NAME}`;
const TX_HASH =
  '907243e63ef4aadc62fe785a1705d1e3fdbd9274206f025f034133ca6a6dea1c';
const BLOCK_HASH =
  '2f7684ce6ea3e07fdd6df7cdbca0607948ecc92f7bae622874ac90ac277a6dfb';
const SLOT = 131545218;

type Call = { url: string; body: string; maxBytes?: number };

const stub = (
  answers: (call: Call) => HttpTransportResult
): HttpTransport & { calls: Array<Call> } => {
  const calls: Array<Call> = [];
  return {
    calls,
    async post(
      url: string,
      body: string,
      timeoutMs: number,
      maxResponseBytes?: number
    ): Promise<HttpTransportResult> {
      const call = { url, body, maxBytes: maxResponseBytes };
      calls.push(call);
      return answers(call);
    },
  };
};

const okBody = (records: Array<unknown>): HttpTransportResult => ({
  ok: true,
  status: 200,
  body: JSON.stringify(records),
});

const assetInfoRecord = (overrides: Record<string, unknown> = {}) => ({
  policy_id: POLICY,
  asset_name: ASSET_NAME,
  fingerprint: 'asset12u525xdusv0rzj08s9jnl2d0ymdhxj5crnd2mp',
  minting_tx_hash: TX_HASH,
  mint_cnt: 1,
  cip68_metadata: null,
  ...overrides,
});

const txCborRecord = (overrides: Record<string, unknown> = {}) => ({
  tx_hash: TX_HASH,
  block_hash: BLOCK_HASH,
  absolute_slot: SLOT,
  block_height: 5078119,
  cbor: '84aa00',
  ...overrides,
});

/** Answers both endpoints, so a case only has to say what differs. */
const happy = () =>
  stub((call) =>
    call.url.includes('asset_info')
      ? okBody([assetInfoRecord()])
      : okBody([txCborRecord()])
  );

const query = (transport: HttpTransport, options = {}) =>
  queryKoiosPointers([SUBJECT], {
    baseUrl: BASE,
    transport,
    retryBackoffMs: 0,
    now: 1_000_000,
    budget: new KoiosRequestBudget(),
    ...options,
  });

describe('koiosEndpoint', () => {
  it('composes a URL against a base carrying a path prefix', () => {
    expect(koiosEndpoint(BASE, 'asset_info', ['policy_id'])).toBe(
      'https://preprod.koios.rest/api/v1/asset_info?select=policy_id'
    );
  });

  it('composes a URL against a base with a trailing slash', () => {
    expect(koiosEndpoint(`${BASE}/`, 'tx_cbor', ['cbor'])).toBe(
      'https://preprod.koios.rest/api/v1/tx_cbor?select=cbor'
    );
  });

  it('answers nothing for the direct option and for no selection', () => {
    expect(koiosEndpoint('direct', 'asset_info', ['policy_id'])).toBeNull();
    expect(koiosEndpoint(null, 'asset_info', ['policy_id'])).toBeNull();
    expect(koiosEndpoint('', 'asset_info', ['policy_id'])).toBeNull();
  });
});

describe('koiosBatches', () => {
  it('keeps a wallet-sized list in one batch', () => {
    const subjects = Array.from({ length: 40 }, (unused, index) => `s${index}`);
    expect(koiosBatches(subjects)).toHaveLength(1);
  });

  it('splits at the batch bound', () => {
    const subjects = Array.from(
      { length: KOIOS_MAX_SUBJECTS_PER_BATCH + 1 },
      (unused, index) => `s${index}`
    );
    const batches = koiosBatches(subjects);
    expect(batches).toHaveLength(2);
    expect(batches[0]).toHaveLength(KOIOS_MAX_SUBJECTS_PER_BATCH);
    expect(batches[1]).toHaveLength(1);
  });
});

describe('koiosBackoffMs', () => {
  it('doubles with each failure and stops at the ceiling', () => {
    expect(koiosBackoffMs(1)).toBe(5 * 60 * 1000);
    expect(koiosBackoffMs(2)).toBe(10 * 60 * 1000);
    expect(koiosBackoffMs(20)).toBe(24 * 60 * 60 * 1000);
  });
});

describe('KoiosRequestBudget', () => {
  it('refuses past the ceiling and lets the window move on', () => {
    const budget = new KoiosRequestBudget(2, 1000);
    expect(budget.tryConsume(0)).toBe(true);
    expect(budget.tryConsume(0)).toBe(true);
    expect(budget.tryConsume(0)).toBe(false);
    expect(budget.tryConsume(1001)).toBe(true);
  });
});

describe('queryKoiosPointers', () => {
  it('resolves a pointer and its transaction in two requests', async () => {
    const transport = happy();
    const result = await query(transport);
    expect(transport.calls).toHaveLength(2);
    expect(result.pointers).toEqual([
      {
        subject: SUBJECT,
        policyId: POLICY,
        assetName: ASSET_NAME,
        fingerprint: 'asset12u525xdusv0rzj08s9jnl2d0ymdhxj5crnd2mp',
        mintingTxHash: TX_HASH,
        mintCount: 1,
        cip68Metadata: null,
      },
    ]);
    expect(result.transactions).toHaveLength(1);
    expect(result.transactions[0].absoluteSlot).toBe(SLOT);
    expect(result.resolutions).toHaveLength(0);
  });

  it('costs two requests for a wallet holding many assets, not two each', async () => {
    const transport = stub((call) =>
      call.url.includes('asset_info')
        ? okBody(
            Array.from({ length: 40 }, (unused, index) =>
              assetInfoRecord({
                asset_name: `${index}`.padStart(4, '0'),
                minting_tx_hash: TX_HASH,
              })
            )
          )
        : okBody([txCborRecord()])
    );
    const subjects = Array.from(
      { length: 40 },
      (unused, index) => `${POLICY}${`${index}`.padStart(4, '0')}`
    );
    await queryKoiosPointers(subjects, {
      baseUrl: BASE,
      transport,
      retryBackoffMs: 0,
      now: 1_000_000,
      budget: new KoiosRequestBudget(),
    });
    expect(transport.calls).toHaveLength(2);
  });

  it('asks for a select list and never for the logo', async () => {
    const transport = happy();
    await query(transport);
    const [info] = transport.calls;
    expect(info.url).toContain(`select=${KOIOS_ASSET_INFO_FIELDS.join(',')}`);
    expect(info.url).not.toContain('logo');
    expect(info.url).not.toContain('minting_tx_metadata');
  });

  it('sends subjects and nothing else', async () => {
    const transport = happy();
    await query(transport);
    expect(JSON.parse(transport.calls[0].body)).toEqual({
      _asset_list: [[POLICY, ASSET_NAME]],
    });
    expect(JSON.parse(transport.calls[1].body)).toEqual({
      _tx_hashes: [TX_HASH],
    });
  });

  it('asks for one transaction when many assets share a minting transaction', async () => {
    const transport = stub((call) =>
      call.url.includes('asset_info')
        ? okBody([assetInfoRecord(), assetInfoRecord({ asset_name: 'beef' })])
        : okBody([txCborRecord()])
    );
    await queryKoiosPointers([SUBJECT, `${POLICY}beef`], {
      baseUrl: BASE,
      transport,
      retryBackoffMs: 0,
      now: 1_000_000,
      budget: new KoiosRequestBudget(),
    });
    expect(JSON.parse(transport.calls[1].body)).toEqual({
      _tx_hashes: [TX_HASH],
    });
  });

  it('issues nothing when the source is the direct option', async () => {
    const transport = happy();
    const result = await query(transport, { baseUrl: 'direct' });
    expect(transport.calls).toHaveLength(0);
    expect(result.pointers).toHaveLength(0);
  });

  it('issues nothing when no source is selected', async () => {
    const transport = happy();
    const result = await query(transport, { baseUrl: null });
    expect(transport.calls).toHaveLength(0);
    expect(result.pointers).toHaveLength(0);
  });

  it('backs off a 429 rather than retrying it', async () => {
    const transport = stub(() => ({ ok: true, status: 429, body: '' }));
    const result = await query(transport);
    expect(transport.calls).toHaveLength(1);
    expect(result.pointers).toHaveLength(0);
    expect(result.resolutions).toEqual([
      {
        subject: SUBJECT,
        state: 'failed',
        failureCount: 0,
        retryAfter: 1_000_000 + KOIOS_THROTTLED_RETRY_MS,
      },
    ]);
  });

  it('leaves rows absent when the per-process ceiling is reached', async () => {
    const transport = happy();
    const budget = new KoiosRequestBudget(0, 1000);
    const result = await query(transport, { budget });
    expect(transport.calls).toHaveLength(0);
    expect(result.pointers).toHaveLength(0);
    expect(result.resolutions[0].retryAfter).toBe(
      1_000_000 + KOIOS_THROTTLED_RETRY_MS
    );
  });

  it('stops asking once the ceiling is reached rather than working through the batches', async () => {
    const transport = happy();
    const budget = new KoiosRequestBudget(1, 1000);
    const subjects = Array.from(
      { length: KOIOS_MAX_SUBJECTS_PER_BATCH * 2 },
      (unused, index) => `${POLICY}${`${index}`.padStart(4, '0')}`
    );
    await queryKoiosPointers(subjects, {
      baseUrl: BASE,
      transport,
      retryBackoffMs: 0,
      now: 1_000_000,
      budget,
    });
    // One request went out, the second was refused, and the second batch was
    // not attempted at all.
    expect(transport.calls).toHaveLength(1);
  });

  it('retries a server error once and then abandons the batch', async () => {
    const transport = stub(() => ({ ok: true, status: 503, body: '' }));
    const result = await query(transport);
    expect(transport.calls).toHaveLength(2);
    expect(result.pointers).toHaveLength(0);
    expect(result.resolutions[0].state).toBe('failed');
    expect(result.resolutions[0].failureCount).toBe(1);
  });

  it('records a retry after a timeout and does not spin', async () => {
    const transport = stub(() => ({ ok: false, reason: 'timeout' }));
    const result = await query(transport);
    expect(transport.calls).toHaveLength(2);
    expect(result.resolutions[0].retryAfter).toBe(
      1_000_000 + koiosBackoffMs(1)
    );
  });

  it('carries an existing failure count into the next backoff', async () => {
    const transport = stub(() => ({ ok: false, reason: 'network' }));
    const result = await query(transport, {
      failureCounts: { [SUBJECT]: 2 },
    });
    expect(result.resolutions[0].failureCount).toBe(3);
    expect(result.resolutions[0].retryAfter).toBe(
      1_000_000 + koiosBackoffMs(3)
    );
  });

  it('abandons a batch whose second call fails and writes no pointer', async () => {
    const transport = stub((call) =>
      call.url.includes('asset_info')
        ? okBody([assetInfoRecord()])
        : { ok: false, reason: 'network' }
    );
    const result = await query(transport);
    expect(result.pointers).toHaveLength(0);
    expect(result.transactions).toHaveLength(0);
    expect(result.resolutions[0].state).toBe('failed');
  });

  it('drops a pointer whose transaction the index did not return', async () => {
    const transport = stub((call) =>
      call.url.includes('asset_info') ? okBody([assetInfoRecord()]) : okBody([])
    );
    const result = await query(transport);
    expect(result.pointers).toHaveLength(0);
    expect(result.transactions).toHaveLength(0);
  });

  it('drops an entry for a subject nobody asked about', async () => {
    const transport = stub((call) =>
      call.url.includes('asset_info')
        ? okBody([assetInfoRecord({ asset_name: 'deadbeef' })])
        : okBody([txCborRecord()])
    );
    const result = await query(transport);
    expect(result.pointers).toHaveLength(0);
    // Nothing to ask for, so the second call is never made.
    expect(transport.calls).toHaveLength(1);
  });

  it('drops a transaction nobody asked about', async () => {
    const transport = stub((call) =>
      call.url.includes('asset_info')
        ? okBody([assetInfoRecord()])
        : okBody([txCborRecord({ tx_hash: 'a'.repeat(64) })])
    );
    const result = await query(transport);
    expect(result.transactions).toHaveLength(0);
    expect(result.pointers).toHaveLength(0);
  });

  it('answers nothing for a body that is not an array', async () => {
    const transport = stub(() => ({
      ok: true,
      status: 200,
      body: JSON.stringify({ asset_info: [] }),
    }));
    const result = await query(transport);
    expect(result.pointers).toHaveLength(0);
    expect(result.resolutions[0].state).toBe('failed');
  });

  it('carries a CIP-68 datum keyed by asset name', async () => {
    const transport = stub((call) =>
      call.url.includes('asset_info')
        ? okBody([
            assetInfoRecord({
              cip68_metadata: { [ASSET_NAME]: { name: 'Northwind Demo' } },
            }),
          ])
        : okBody([txCborRecord()])
    );
    const result = await query(transport);
    expect(result.pointers[0].cip68Metadata).toEqual({
      name: 'Northwind Demo',
    });
  });

  it('names the response cap it will read up to', async () => {
    const transport = happy();
    await query(transport);
    expect(transport.calls[0].maxBytes).toBe(8 * 1024 * 1024);
  });

  it('takes a CIP-68 map that is not keyed by asset name as the record itself', async () => {
    const transport = stub((call) =>
      call.url.includes('asset_info')
        ? okBody([assetInfoRecord({ cip68_metadata: { name: 'Flat' } })])
        : okBody([txCborRecord()])
    );
    const result = await query(transport);
    expect(result.pointers[0].cip68Metadata).toEqual({ name: 'Flat' });
  });

  it('drops a transaction record that carries no bytes', async () => {
    const transport = stub((call) =>
      call.url.includes('asset_info')
        ? okBody([assetInfoRecord()])
        : okBody([txCborRecord({ cbor: null })])
    );
    const result = await query(transport);
    expect(result.transactions).toHaveLength(0);
    expect(result.pointers).toHaveLength(0);
  });

  it('drops a transaction record with no absolute slot', async () => {
    const transport = stub((call) =>
      call.url.includes('asset_info')
        ? okBody([assetInfoRecord()])
        : okBody([txCborRecord({ absolute_slot: null })])
    );
    const result = await query(transport);
    expect(result.transactions).toHaveLength(0);
  });

  it('drops an entry that is not an object', async () => {
    const transport = stub((call) =>
      call.url.includes('asset_info') ? okBody(['not a record']) : okBody([])
    );
    const result = await query(transport);
    expect(result.pointers).toHaveLength(0);
    expect(transport.calls).toHaveLength(1);
  });

  it('drops an entry with no minting transaction hash', async () => {
    const transport = stub((call) =>
      call.url.includes('asset_info')
        ? okBody([assetInfoRecord({ minting_tx_hash: null })])
        : okBody([])
    );
    const result = await query(transport);
    expect(result.pointers).toHaveLength(0);
  });

  // The ceiling is checked before every request including the second of a
  // batch, so a batch can be throttled halfway through.
  it('throttles the second call of a batch when the ceiling falls between them', async () => {
    const transport = happy();
    const budget = new KoiosRequestBudget(1, 1000);
    const result = await query(transport, { budget });
    expect(transport.calls).toHaveLength(1);
    expect(result.pointers).toHaveLength(0);
    expect(result.resolutions[0].retryAfter).toBe(
      1_000_000 + KOIOS_THROTTLED_RETRY_MS
    );
  });

  it('answers nothing for an empty subject list', async () => {
    const transport = happy();
    const result = await queryKoiosPointers([], {
      baseUrl: BASE,
      transport,
      budget: new KoiosRequestBudget(),
    });
    expect(transport.calls).toHaveLength(0);
    expect(result).toEqual({ pointers: [], transactions: [], resolutions: [] });
  });

  it('drops a transaction entry that is not an object', async () => {
    const transport = stub((call) =>
      call.url.includes('asset_info')
        ? okBody([assetInfoRecord()])
        : okBody(['not a record'])
    );
    const result = await query(transport);
    expect(result.transactions).toHaveLength(0);
  });

  it('waits the backoff it was given before retrying', async () => {
    const transport = stub(() => ({ ok: true, status: 503, body: '' }));
    const started = Date.now();
    await query(transport, { retryBackoffMs: 20 });
    expect(Date.now() - started).toBeGreaterThanOrEqual(15);
    expect(transport.calls).toHaveLength(2);
  });

  // The ceiling is consulted again before the retry, so a batch can run out of
  // room between its first attempt and its second.
  it('throttles rather than retrying when the ceiling falls before the retry', async () => {
    const transport = stub(() => ({ ok: true, status: 503, body: '' }));
    const budget = new KoiosRequestBudget(1, 1000);
    const result = await query(transport, { budget });
    expect(transport.calls).toHaveLength(1);
    expect(result.resolutions[0].retryAfter).toBe(
      1_000_000 + KOIOS_THROTTLED_RETRY_MS
    );
  });

  it('abandons a batch whose body is not JSON at all', async () => {
    const transport = stub(() => ({
      ok: true,
      status: 200,
      body: '<html>gateway</html>',
    }));
    const result = await query(transport);
    expect(result.pointers).toHaveLength(0);
    expect(result.resolutions[0].state).toBe('failed');
  });
});
