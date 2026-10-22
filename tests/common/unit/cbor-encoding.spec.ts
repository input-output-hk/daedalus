/** @jest-environment node */

import { PassThrough } from 'stream';

const cbor = require('cbor');

describe('synchronous CBOR encoding', () => {
  it('collects complete arrays and canonical map keys', () => {
    expect(cbor.encodeCanonical([1, 2, 3]).toString('hex')).toBe('83010203');
    expect(
      cbor
        .encodeCanonical(
          new Map([
            [3, 4],
            [1, 2],
          ])
        )
        .toString('hex')
    ).toBe('a201020304');
    expect(
      cbor
        .encodeCanonical(
          new Map([
            ['long', 2],
            ['a', 1],
          ])
        )
        .toString('hex')
    ).toBe('a2616101646c6f6e6702');
  });

  it('preserves bytes beyond stream high water marks', () => {
    const payload = Buffer.alloc(65536, 0xa5);
    const encoded = cbor.encodeOne([payload, '日本語'], {
      canonical: true,
      highWaterMark: 1,
    });
    expect(encoded.subarray(0, 6).toString('hex')).toBe('825a00010000');
    expect(cbor.decodeFirstSync(encoded)).toEqual([payload, '日本語']);
    expect(cbor.encode(1, 2, 3).toString('hex')).toBe('010203');
  });

  it('retains asynchronous streaming encoding', async () => {
    const encoder = new cbor.Encoder({ canonical: true });
    const output = new PassThrough();
    const chunks: Buffer[] = [];
    output.on('data', (chunk: Buffer) => chunks.push(chunk));
    const finished = new Promise<void>((resolve, reject) => {
      output.on('end', resolve);
      output.on('error', reject);
      encoder.on('error', reject);
    });
    encoder.pipe(output);
    encoder.write([1, 2, 3]);
    encoder.end(
      new Map([
        [3, 4],
        [1, 2],
      ])
    );
    await finished;
    expect(Buffer.concat(chunks).toString('hex')).toBe('83010203a201020304');
  });
});
