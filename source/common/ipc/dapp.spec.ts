import fs from 'fs';
import path from 'path';
import contractManifest from '../cip30/contracts/contract-manifest.json';
import { DAPP_CIP30_METHODS, parseDappConsentRender } from './dapp';

const preloadPath = path.resolve(__dirname, '../../main/preloads/dapp.ts');
const webpackPath = path.resolve(__dirname, '../../main/webpack.config.js');

describe('dApp preload contract', () => {
  it('keeps the gateway dispatch surface equal to the frozen manifest', () => {
    expect(DAPP_CIP30_METHODS).toEqual(
      contractManifest.methods.map(({ path: methodPath }) => methodPath)
    );
  });

  it('imports no privileged trusted-preload capabilities', () => {
    const source = fs.readFileSync(preloadPath, 'utf8');
    expect(source.match(/contextBridge\.exposeInMainWorld/g)).toHaveLength(1);
    expect(source).not.toMatch(
      /(?:from|require\()['"](?:\.\.\/preload|https?|os|fs|path|electron-log-daedalus|\.\.\/config|\.\.\/environment)/
    );
    expect(source).not.toMatch(/Object\.assign\s*\(\s*global|global\./);
  });

  it('builds the dApp preload as one dedicated entry', () => {
    const config = require(webpackPath);
    expect(config.entry.dapp).toBe('./source/main/preloads/dapp.ts');
    expect(config.optimization).toEqual(
      expect.objectContaining({ splitChunks: false, runtimeChunk: false })
    );
  });

  it('accepts only display-safe consent presentations', () => {
    expect(
      parseDappConsentRender({
        type: 'present',
        request: {
          requestId: 'request',
          kind: 'connection',
          origin: 'https://example.test',
          walletName: 'Wallet',
          networkName: 'Preview',
          scopes: ['connection', 'read'],
          extensions: [95],
        },
      })
    ).toEqual(
      expect.objectContaining({
        type: 'present',
        request: expect.objectContaining({ requestId: 'request' }),
      })
    );
    expect(() =>
      parseDappConsentRender({
        type: 'present',
        request: {
          requestId: 'request',
          kind: 'connection',
          origin: 'https://example.test',
          walletName: 'Wallet',
          networkName: 'Preview',
          scopes: ['connection'],
          extensions: [],
          args: ['replacement'],
        },
      })
    ).toThrow('Invalid dApp consent presentation');
  });
});
