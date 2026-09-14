/**
 * The renderer clients for the asset metadata channels, against a fake
 * `ipcRenderer` that answers in whatever order the case chooses.
 *
 * The property under test is correlation. `IpcChannel` resolves a request on
 * the next message to reach the channel's single response name, so two reads in
 * flight can each take the other's answer, and the answer here feeds decimal
 * places.
 */
import {
  assetImageChannel,
  assetMetadataChannel,
  onAssetMetadataUpdate,
  requestAssetImage,
  requestAssetMetadata,
} from './assetMetadataChannel';

type Sent = { channel: string; message: any };
type Listener = { channel: string; handler: (...args: Array<any>) => any };

const sent: Array<Sent> = [];
const onceListeners: Array<Listener> = [];
const onListeners: Array<Listener> = [];

const fakeIpcRenderer = {
  send: (channel: string, message: any) => {
    sent.push({ channel, message });
  },
  once: (channel: string, handler: (...args: Array<any>) => any) => {
    onceListeners.push({ channel, handler });
  },
  on: (channel: string, handler: (...args: Array<any>) => any) => {
    onListeners.push({ channel, handler });
  },
  removeListener: (channel: string, handler: (...args: Array<any>) => any) => {
    const index = onListeners.findIndex(
      (listener) => listener.channel === channel && listener.handler === handler
    );
    if (index >= 0) onListeners.splice(index, 1);
  },
};

/**
 * Electron fires one-shot listeners in registration order and unregisters each
 * as it fires, whatever the message it fired on. That ordering is the whole
 * defect, so the fake reproduces it rather than matching messages to listeners.
 */
const answer = (channel: string, payload: any, isOk = true) => {
  const index = onceListeners.findIndex(
    (listener) => listener.channel === channel
  );
  if (index < 0) throw new Error(`nothing is listening on ${channel}`);
  const [listener] = onceListeners.splice(index, 1);
  listener.handler({ sender: fakeIpcRenderer }, isOk, payload);
};

const METADATA_RESPONSE_CHANNEL = 'ASSET_METADATA_CHANNEL-response';
const METADATA_REQUEST_CHANNEL = 'ASSET_METADATA_CHANNEL-request';
const IMAGE_RESPONSE_CHANNEL = 'ASSET_IMAGE_CHANNEL-response';
const UPDATE_BROADCAST_CHANNEL = 'ASSET_METADATA_UPDATE_CHANNEL-broadcast';

const FIRST = `${'a'.repeat(56)}01`;
const SECOND = `${'b'.repeat(56)}02`;

const metadataResponse = (requestId: string, subject: string) => ({
  requestId,
  entries: [{ subject, ticker: subject.slice(-2) }],
  unresolved: [],
});

const idsSentOn = (channel: string) =>
  sent
    .filter((message) => message.channel === channel)
    .map((message) => message.message.requestId);

const settled = () => new Promise((resolve) => setTimeout(resolve, 0));

describe('assetMetadataChannel', () => {
  beforeEach(() => {
    sent.length = 0;
    onceListeners.length = 0;
    onListeners.length = 0;
    // The renderer channels default their sender and receiver to this global.
    (global as any).ipcRenderer = fakeIpcRenderer;
  });

  describe('requestAssetMetadata', () => {
    it('sends one request carrying the id it minted and the subjects it was given', async () => {
      const pending = requestAssetMetadata([FIRST]);
      expect(sent).toHaveLength(1);
      expect(sent[0].channel).toBe(METADATA_REQUEST_CHANNEL);
      expect(sent[0].message.subjects).toEqual([FIRST]);
      const { requestId } = sent[0].message;
      expect(typeof requestId).toBe('string');
      answer(METADATA_RESPONSE_CHANNEL, metadataResponse(requestId, FIRST));
      await expect(pending).resolves.toEqual(
        metadataResponse(requestId, FIRST)
      );
    });

    it('asks for an ordinary read unless a refresh is asked for', () => {
      requestAssetMetadata([FIRST]);
      expect(sent[0].message.refresh).toBe(false);
    });

    it('carries the refresh flag to the main process when one is asked for', () => {
      requestAssetMetadata([FIRST], { refresh: true });
      expect(sent[0].message.refresh).toBe(true);
      expect(sent[0].message.subjects).toEqual([FIRST]);
    });

    it('mints a different id for every request', () => {
      requestAssetMetadata([FIRST]);
      requestAssetMetadata([SECOND]);
      const [first, second] = idsSentOn(METADATA_REQUEST_CHANNEL);
      expect(first).not.toBe(second);
    });

    it('gives each of two overlapping requests the answer it asked for, in whatever order they arrive', async () => {
      const firstPending = requestAssetMetadata([FIRST]);
      const secondPending = requestAssetMetadata([SECOND]);
      const [firstId, secondId] = idsSentOn(METADATA_REQUEST_CHANNEL);

      // The second request is answered first, which is the case the one-shot
      // listener gets wrong.
      answer(METADATA_RESPONSE_CHANNEL, metadataResponse(secondId, SECOND));
      answer(METADATA_RESPONSE_CHANNEL, metadataResponse(firstId, FIRST));

      await expect(firstPending).resolves.toEqual(
        metadataResponse(firstId, FIRST)
      );
      await expect(secondPending).resolves.toEqual(
        metadataResponse(secondId, SECOND)
      );
    });

    it('is not decorative: the raw channel swaps the same two answers', async () => {
      const firstPending = assetMetadataChannel.request({
        requestId: 'raw-first',
        subjects: [FIRST],
      });
      const secondPending = assetMetadataChannel.request({
        requestId: 'raw-second',
        subjects: [SECOND],
      });

      answer(METADATA_RESPONSE_CHANNEL, metadataResponse('raw-second', SECOND));
      answer(METADATA_RESPONSE_CHANNEL, metadataResponse('raw-first', FIRST));

      // Each promise takes the payload meant for the other. This is what the
      // registry in the client exists to prevent, and it is the control that
      // stops the case above passing for the wrong reason.
      await expect(firstPending).resolves.toEqual(
        metadataResponse('raw-second', SECOND)
      );
      await expect(secondPending).resolves.toEqual(
        metadataResponse('raw-first', FIRST)
      );
    });

    it('discards a response it did not issue and keeps waiting', async () => {
      const pending = requestAssetMetadata([FIRST]);
      const [requestId] = idsSentOn(METADATA_REQUEST_CHANNEL);
      let resolved = false;
      pending.then(() => {
        resolved = true;
      });

      answer(
        METADATA_RESPONSE_CHANNEL,
        metadataResponse('an-id-nobody-issued', SECOND)
      );
      await settled();
      expect(resolved).toBe(false);

      // The listener the stray response consumed is gone, so the real answer
      // needs a listener of its own: this is what a second overlapping request
      // supplies in practice.
      requestAssetMetadata([SECOND]);
      answer(METADATA_RESPONSE_CHANNEL, metadataResponse(requestId, FIRST));
      await expect(pending).resolves.toEqual(
        metadataResponse(requestId, FIRST)
      );
    });

    it('leaves a request pending rather than expiring it', async () => {
      const pending = requestAssetMetadata([FIRST]);
      let resolved = false;
      pending.then(() => {
        resolved = true;
      });
      await settled();
      await settled();
      expect(resolved).toBe(false);
    });
  });

  describe('requestAssetImage', () => {
    it('gives each of two overlapping requests its own answer', async () => {
      const firstPending = requestAssetImage(FIRST);
      const secondPending = requestAssetImage(SECOND);
      const [firstId, secondId] = idsSentOn('ASSET_IMAGE_CHANNEL-request');

      answer(IMAGE_RESPONSE_CHANNEL, {
        requestId: secondId,
        status: 'absent',
      });
      answer(IMAGE_RESPONSE_CHANNEL, {
        requestId: firstId,
        status: 'present',
        mediaType: 'image/png',
        bytes: new Uint8Array([1, 2, 3]),
      });

      const first = await firstPending;
      const second = await secondPending;
      expect(first.status).toBe('present');
      expect(first.requestId).toBe(firstId);
      expect(second.status).toBe('absent');
      expect(second.requestId).toBe(secondId);
    });

    it('sends one subject per request', () => {
      requestAssetImage(FIRST);
      expect(sent).toHaveLength(1);
      expect(sent[0].message.subject).toBe(FIRST);
    });

    it('does not answer a request from another channel', async () => {
      const pending = requestAssetImage(FIRST);
      const [imageId] = idsSentOn('ASSET_IMAGE_CHANNEL-request');
      let resolved = false;
      pending.then(() => {
        resolved = true;
      });

      requestAssetMetadata([FIRST]);
      const [metadataId] = idsSentOn(METADATA_REQUEST_CHANNEL);
      answer(METADATA_RESPONSE_CHANNEL, metadataResponse(metadataId, FIRST));
      await settled();
      expect(resolved).toBe(false);

      answer(IMAGE_RESPONSE_CHANNEL, {
        requestId: imageId,
        status: 'absent',
      });
      await expect(pending).resolves.toEqual({
        requestId: imageId,
        status: 'absent',
      });
    });
  });

  describe('onAssetMetadataUpdate', () => {
    it('delivers a pushed message to the subscribed handler', async () => {
      const received: Array<any> = [];
      onAssetMetadataUpdate((message) => received.push(message));
      const listener = onListeners.find(
        (candidate) => candidate.channel === UPDATE_BROADCAST_CHANNEL
      );
      expect(listener).toBeDefined();
      const entries = [{ subject: FIRST, ticker: 'ONE' }];
      await listener.handler({ sender: fakeIpcRenderer }, { entries });
      expect(received).toEqual([{ entries }]);
    });
  });

  describe('the channel declarations', () => {
    it('derives its three wire names from the shared constant', () => {
      expect(assetImageChannel).toBeDefined();
      requestAssetImage(FIRST);
      expect(sent[0].channel).toBe('ASSET_IMAGE_CHANNEL-request');
    });
  });
});
