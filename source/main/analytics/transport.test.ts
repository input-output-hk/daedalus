/** @jest-environment node */
import { EventEmitter } from 'events';
import http, { ClientRequest, IncomingMessage } from 'http';
import https from 'https';
import timers from 'timers';
import { postAnalytics } from './transport';

afterEach(() => {
  jest.restoreAllMocks();
  jest.useRealTimers();
});
function mockRequest(protocol: Pick<typeof http, 'request'> = https) {
  let respond: (response: IncomingMessage) => void;
  const request = Object.assign(new EventEmitter(), {
    end: jest.fn(),
    destroy: jest.fn(),
  });
  const spy = jest.spyOn(protocol, 'request').mockImplementation(((
    _url: unknown,
    _options: unknown,
    callback: (response: IncomingMessage) => void
  ) => {
    respond = callback;
    return request as unknown as ClientRequest;
  }) as typeof http.request);
  return {
    request,
    spy,
    respond(status: number, headers = {}) {
      const response = { statusCode: status, headers, destroy: jest.fn() };
      respond(response as unknown as IncomingMessage);
      return response;
    },
  };
}
test('HTTP adapter sends one JSON POST with no credentials/cookies and never follows redirects or reads bodies', async () => {
  const mock = mockRequest();
  const controller = new AbortController();
  const result = postAnalytics(
    'https://example.invalid/api/analytics/event',
    '{"synthetic":true}',
    controller.signal
  );
  const response = mock.respond(302, {
    location: 'https://another.invalid',
    'retry-after': '2',
  });
  expect(await result).toEqual({ status: 302, retryAfter: 2 });
  expect(mock.spy).toHaveBeenCalledTimes(1);
  expect(mock.spy.mock.calls[0][1]).toMatchObject({
    method: 'POST',
    agent: false,
    maxHeaderSize: 8192,
    headers: { 'content-type': 'application/json', 'content-length': 18 },
  });
  expect(Object.keys(mock.spy.mock.calls[0][1].headers)).toEqual([
    'content-type',
    'content-length',
  ]);
  expect(response.destroy).toHaveBeenCalled();
  expect(mock.request.destroy).toHaveBeenCalled();
});
test('total deadline and revocation destroy sockets; pre-aborted requests do not start', async () => {
  let deadline: () => void;
  const timer = jest.spyOn(timers, 'setTimeout').mockImplementation(((
    callback: () => void
  ) => {
    deadline = callback;
    return { unref: jest.fn() } as unknown as NodeJS.Timeout;
  }) as typeof timers.setTimeout);
  jest.spyOn(timers, 'clearTimeout').mockImplementation(() => undefined);
  const mock = mockRequest(http);
  const controller = new AbortController();
  const result = postAnalytics(
    'http://127.0.0.1/api/analytics/event',
    '{}',
    controller.signal
  );
  expect(timer).toHaveBeenCalledWith(expect.any(Function), 5000);
  deadline();
  expect(await result).toEqual({ status: 0 });
  expect(mock.request.destroy).toHaveBeenCalledTimes(1);
  const second = postAnalytics(
    'http://127.0.0.1/api/analytics/event',
    '{}',
    controller.signal
  );
  controller.abort();
  expect(await second).toEqual({ status: 0 });
  expect(
    await postAnalytics(
      'http://127.0.0.1/api/analytics/event',
      '{}',
      controller.signal
    )
  ).toEqual({ status: 0 });
  expect(mock.spy).toHaveBeenCalledTimes(2);
});
test('provider errors become an empty status; arbitrary Retry-After text is discarded', async () => {
  const mock = mockRequest();
  const controller = new AbortController();
  const first = postAnalytics(
    'https://example.invalid/api/analytics/event',
    '{}',
    controller.signal
  );
  mock.request.emit('error', new Error('private provider error'));
  expect(await first).toEqual({ status: 0 });
  const second = postAnalytics(
    'https://example.invalid/api/analytics/event',
    '{}',
    controller.signal
  );
  mock.respond(429, { 'retry-after': 'provider text' });
  expect(await second).toEqual({ status: 429, retryAfter: undefined });
});
