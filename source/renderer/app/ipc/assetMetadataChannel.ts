import { v4 as uuidv4 } from 'uuid';
import { RendererIpcChannel } from './lib/RendererIpcChannel';
import {
  ASSET_IMAGE_CHANNEL,
  ASSET_METADATA_CHANNEL,
  ASSET_METADATA_UPDATE_CHANNEL,
} from '../../../common/ipc/api';
import type {
  AssetImageMainResponse,
  AssetImageRendererRequest,
  AssetMetadataMainResponse,
  AssetMetadataRendererRequest,
  AssetMetadataUpdateMainRequest,
  AssetMetadataUpdateRendererResponse,
} from '../../../common/ipc/api';

export const assetMetadataChannel: RendererIpcChannel<
  AssetMetadataMainResponse,
  AssetMetadataRendererRequest
> = new RendererIpcChannel(ASSET_METADATA_CHANNEL);

export const assetMetadataUpdateChannel: RendererIpcChannel<
  AssetMetadataUpdateMainRequest,
  AssetMetadataUpdateRendererResponse
> = new RendererIpcChannel(ASSET_METADATA_UPDATE_CHANNEL);

export const assetImageChannel: RendererIpcChannel<
  AssetImageMainResponse,
  AssetImageRendererRequest
> = new RendererIpcChannel(ASSET_IMAGE_CHANNEL);

type Correlated = { requestId: string };

/**
 * Waiters for one channel, keyed by the id each of them issued.
 *
 * `IpcChannel.request` registers a one-shot listener on the channel's single
 * response name and resolves on the next message to arrive, whatever request
 * that message answers. Two reads in flight are two listeners on one stream,
 * fired in registration order by arrival order, so a response can land on the
 * wrong promise.
 *
 * Correlation therefore cannot live inside one call. A call that checked the id
 * on its own promise and kept waiting would wait forever, because the message it
 * wanted was already consumed by the other listener. The waiters share a
 * registry instead: whichever promise settles hands the payload to the waiter
 * whose id it carries, and a payload nobody is waiting for is discarded.
 */
const deliver = <TResponse extends Correlated>(
  waiters: Map<string, (response: TResponse) => void>,
  response: TResponse
): void => {
  const requestId = response?.requestId;
  const waiter = typeof requestId === 'string' ? waiters.get(requestId) : null;
  if (!waiter) return;
  waiters.delete(requestId);
  waiter(response);
};

const metadataWaiters = new Map<
  string,
  (response: AssetMetadataMainResponse) => void
>();

const imageWaiters = new Map<
  string,
  (response: AssetImageMainResponse) => void
>();

/**
 * Asks for the rows the cache holds for these subjects and returns what it has.
 * Subjects it has no row for come back under `unresolved`, and resolution for
 * them is scheduled in the main process, so the answer is never behind a
 * request to the registry.
 *
 * `refresh` asks the main process to schedule these subjects whether or not
 * their refresh window has elapsed and whether or not they are inside a retry
 * backoff. It does not change what comes back now, only what is fetched next.
 *
 * `sourceUrl` is the user's selected pointer source. It travels with every read
 * because the setting lives here and the client that uses it lives there.
 */
export const requestAssetMetadata = (
  subjects: Array<string>,
  options: { refresh?: boolean; sourceUrl?: string | null } = {}
): Promise<AssetMetadataMainResponse> =>
  new Promise((resolve) => {
    const requestId = uuidv4();
    metadataWaiters.set(requestId, resolve);
    assetMetadataChannel
      .request({
        requestId,
        subjects,
        refresh: options.refresh === true,
        sourceUrl: options.sourceUrl ?? null,
      })
      .then((response) => deliver(metadataWaiters, response))
      // A rejected response arrives without an id, so it cannot be handed to the
      // waiter it belongs to. The main handlers answer on every path and never
      // reject, which is what keeps this unreachable; rejecting some other
      // waiter to be rid of it would be worse than leaving this one waiting.
      .catch(() => {});
  });

/** Asks for one subject's logo. Answers `absent` rather than failing. */
export const requestAssetImage = (
  subject: string
): Promise<AssetImageMainResponse> =>
  new Promise((resolve) => {
    const requestId = uuidv4();
    imageWaiters.set(requestId, resolve);
    assetImageChannel
      .request({
        requestId,
        subject,
      })
      .then((response) => deliver(imageWaiters, response))
      .catch(() => {});
  });

/**
 * One `data:` URL per subject, for the life of the renderer.
 *
 * A token list unmounts and remounts its rows as the user scrolls, sorts and
 * searches, so a request held by the row that made it is a request per scroll.
 * The promise is memoised rather than its result, so two rows mounting in the
 * same frame share one request instead of issuing two and discarding one.
 *
 * `null` is remembered as firmly as a URL is. A wallet holding many tokens the
 * registry has no picture for is the ordinary case, and asking again each time
 * such a row is drawn is the cost this map exists to avoid.
 */
const imageUrls = new Map<string, Promise<string | null>>();

const dataUrl = (mediaType: string, bytes: Uint8Array): string =>
  `data:${mediaType};base64,${Buffer.from(bytes).toString('base64')}`;

/**
 * The logo for one subject, as something an `img` can render, or `null` when
 * there is none. Never rejects: the main handler answers `absent` on every
 * failure, and a missing picture is not a condition a row should have to handle.
 */
export const requestAssetImageUrl = (
  subject: string
): Promise<string | null> => {
  const existing = imageUrls.get(subject);
  if (existing) return existing;
  const pending = requestAssetImage(subject).then((response) =>
    // Narrowed by the literal rather than by truthiness: `strict` is off, so a
    // check on the absence of a property does not narrow this union at all.
    response.status === 'present'
      ? dataUrl(response.mediaType, response.bytes)
      : null
  );
  imageUrls.set(subject, pending);
  return pending;
};

/** Subscribes to rows the main process resolves after the fact. */
export const onAssetMetadataUpdate = (
  handler: (message: AssetMetadataUpdateMainRequest) => void
): void => {
  assetMetadataUpdateChannel.onReceive((message) => {
    handler(message);
    return Promise.resolve();
  });
};
