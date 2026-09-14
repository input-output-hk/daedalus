import type { BrowserWindow } from 'electron';
import { MainIpcChannel } from './lib/MainIpcChannel';
import {
  ASSET_IMAGE_CHANNEL,
  ASSET_METADATA_CHANNEL,
  ASSET_METADATA_UPDATE_CHANNEL,
} from '../../common/ipc/api';
import type {
  AssetImageMainResponse,
  AssetImageRendererRequest,
  AssetMetadataMainResponse,
  AssetMetadataRendererRequest,
  AssetMetadataUpdateMainRequest,
  AssetMetadataUpdateRendererResponse,
} from '../../common/ipc/api';
import type {
  AssetMetadataEntry,
  AssetUnresolvedSubject,
} from '../../common/types/asset-metadata.types';
import {
  AssetMetadataDatabase,
  openAssetMetadataDatabase,
} from '../assets/assetMetadataDb';
import type { AssetMetadataRow } from '../assets/assetMetadataDb';
import { AssetMetadataResolver } from '../assets/assetMetadataResolver';
import { AssetImageStore } from '../assets/assetImageStore';
import type { RegistryTransport } from '../assets/assetRegistryClient';
import { logger } from '../utils/logging';

const assetMetadataChannel: MainIpcChannel<
  AssetMetadataRendererRequest,
  AssetMetadataMainResponse
> = new MainIpcChannel(ASSET_METADATA_CHANNEL);

const assetMetadataUpdateChannel: MainIpcChannel<
  AssetMetadataUpdateRendererResponse,
  AssetMetadataUpdateMainRequest
> = new MainIpcChannel(ASSET_METADATA_UPDATE_CHANNEL);

const assetImageChannel: MainIpcChannel<
  AssetImageRendererRequest,
  AssetImageMainResponse
> = new MainIpcChannel(ASSET_IMAGE_CHANNEL);

export type AssetMetadataChannelOptions = {
  window?: BrowserWindow;
  database?: AssetMetadataDatabase;
  transport?: RegistryTransport;
  endpoint?: string | null;
};

/**
 * The stored column is TEXT holding JSON and the entry carries an object.
 * Anything that does not parse to one becomes null: a handler is not the place
 * to discover that a column holds something unexpected.
 */
const parsedMetadata = (
  value: string | null
): Record<string, unknown> | null => {
  if (typeof value !== 'string' || value.length === 0) return null;
  try {
    const parsed = JSON.parse(value);
    if (!parsed || typeof parsed !== 'object' || Array.isArray(parsed)) {
      return null;
    }
    return parsed as Record<string, unknown>;
  } catch {
    return null;
  }
};

const toEntry = (
  row: AssetMetadataRow,
  withImage: Set<string>
): AssetMetadataEntry => ({
  subject: row.subject,
  policyId: row.policyId,
  assetName: row.assetName,
  ticker: row.ticker,
  name: row.name,
  decimals: row.decimals,
  verified: row.verified,
  source: row.source,
  hasImage: withImage.has(row.subject),
  metadata: parsedMetadata(row.metadata),
});

/**
 * One bound parameter per subject and one map entry per subject, and the caller
 * is thirty components each naming the assets it is about to draw. Duplicates
 * collapse and anything that is not a subject is dropped here rather than in
 * the database.
 */
const cleanSubjects = (subjects: unknown): Array<string> => {
  if (!Array.isArray(subjects)) return [];
  const seen = new Set<string>();
  subjects.forEach((subject) => {
    if (typeof subject === 'string' && subject.length > 0) seen.add(subject);
  });
  return Array.from(seen);
};

export class AssetMetadataChannelHandlers {
  private _window?: BrowserWindow;

  private _database: AssetMetadataDatabase;

  private _resolver: AssetMetadataResolver;

  private _images: AssetImageStore;

  constructor(options: AssetMetadataChannelOptions = {}) {
    this._window = options.window;
    // One handle, two consumers. Letting the resolver and the image store each
    // default to their own would open the same file twice from one process.
    this._database = options.database ?? openAssetMetadataDatabase();
    this._resolver = new AssetMetadataResolver({
      database: this._database,
      transport: options.transport,
      endpoint: options.endpoint,
      onResolved: (rows) => this.push(rows),
    });
    this._images = new AssetImageStore({
      database: this._database,
      transport: options.transport,
      endpoint: options.endpoint,
    });
  }

  /**
   * Answers from what the cache holds now and schedules resolution for what it
   * lacks. Nothing here awaits the registry: `request` reads the database and
   * queues the fetch behind the answer.
   */
  readMetadata = async (
    request: AssetMetadataRendererRequest
  ): Promise<AssetMetadataMainResponse> => {
    const requestId = request?.requestId;
    const subjects = cleanSubjects(request?.subjects);
    try {
      const rows = this._resolver.request(subjects, {
        force: request?.refresh === true,
      });
      const withImage = new Set(
        this._database.readImageSubjects(rows.map((row) => row.subject))
      );
      return {
        requestId,
        entries: rows.map((row) => toEntry(row, withImage)),
        unresolved: this._unresolved(subjects, rows),
      };
    } catch (error) {
      // A handler that rejects gives the renderer a response it cannot attribute
      // to a request, which is worse than an empty answer.
      logger.warn('Asset metadata IPC: read failed', {
        reason: error instanceof Error ? error.message : 'unknown',
        subjectCount: subjects.length,
      });
      return { requestId, entries: [], unresolved: [] };
    }
  };

  /**
   * One subject, and the only call in this module that may wait. A logo that
   * takes the full timeout delays a picture and never a row, a name or an
   * amount, which is the reason the channel is separate.
   */
  readImage = async (
    request: AssetImageRendererRequest
  ): Promise<AssetImageMainResponse> => {
    const requestId = request?.requestId;
    try {
      const row = await this._images.fetch(request?.subject);
      if (!row) return { requestId, status: 'absent' };
      return {
        requestId,
        status: 'present',
        mediaType: row.mediaType,
        bytes: row.bytes,
      };
    } catch (error) {
      logger.debug('Asset metadata IPC: image read failed', {
        reason: error instanceof Error ? error.message : 'unknown',
      });
      return { requestId, status: 'absent' };
    }
  };

  /** Rows the resolver has just written, pushed without anyone asking again. */
  push = (rows: Array<AssetMetadataRow>): void => {
    const window = this._window;
    if (!window || window.isDestroyed()) return;
    const withImage = new Set(
      this._database.readImageSubjects(rows.map((row) => row.subject))
    );
    assetMetadataUpdateChannel
      .send(
        { entries: rows.map((row) => toEntry(row, withImage)) },
        window.webContents
      )
      .catch(() => {
        // The renderer answers every push; a window that went away mid-flight
        // is not a failure worth reporting.
      });
  };

  private _unresolved(
    subjects: Array<string>,
    rows: Array<AssetMetadataRow>
  ): Array<AssetUnresolvedSubject> {
    const resolved = new Set(rows.map((row) => row.subject));
    const missing = subjects.filter((subject) => !resolved.has(subject));
    if (missing.length === 0) return [];
    const states = new Map(
      this._database
        .readResolutions(missing)
        .map((row) => [row.subject, row.state])
    );
    // No resolution row means nothing has looked yet, and this request has just
    // scheduled it, so `pending` is true by the time the response is sent.
    return missing.map((subject) => ({
      subject,
      state: states.get(subject) ?? 'pending',
    }));
  }
}

let registered = false;

/**
 * Registering twice would answer one request with two responses, and the second
 * of them would be taken by another request's one-shot listener, leaving that
 * request with nothing left to resolve it.
 */
export const handleAssetMetadataRequests = (
  window: BrowserWindow,
  options: AssetMetadataChannelOptions = {}
): AssetMetadataChannelHandlers | null => {
  if (registered) return null;
  registered = true;
  const handlers = new AssetMetadataChannelHandlers({ window, ...options });
  assetMetadataChannel.onRequest(handlers.readMetadata);
  assetImageChannel.onRequest(handlers.readImage);
  return handlers;
};
