import fs from 'fs';
import path from 'path';
import contractManifest from '../cip30/contracts/contract-manifest.json';
import { DAPP_CIP30_METHODS } from '../cip30/wire';
import { parseWalletApprovalRender } from './walletApproval';

const preloadPath = path.resolve(__dirname, '../../main/preloads/dapp.ts');
const webpackPath = path.resolve(__dirname, '../../main/webpack.config.js');

const walletId = 'aa'.repeat(20);
const transactionReview = {
  mode: 'sign',
  transactionId: '11'.repeat(32),
  bodyCbor: 'a0',
  fullCbor: '84a0a0f5f6',
  fullCborDigest: '22'.repeat(32),
  witnessSetCbor: 'a0',
  auxiliaryDataCbor: 'f6',
  isValid: true,
  display: {
    entries: [],
    walletInputs: null,
    walletOutputs: null,
    walletChange: null,
    fee: '0',
    deposits: null,
    refunds: null,
    maximumCollateralLoss: null,
    mint: [],
    withdrawals: [],
    certificates: [],
    votes: [],
    proposalCount: 0,
    donation: null,
  },
  effects: [{ index: 0, kind: 'input', value: '{}' }],
  existingVkeyWitnesses: [],
  existingBootstrapWitnesses: [],
  commitmentsVerified: true,
  approvable: true,
  refusalReasons: [],
};

const batchPresentation = {
  requestId: 'batch',
  walletId,
  kind: 'batch-sign',
  origin: 'https://example.test',
  walletName: 'Wallet',
  networkName: 'Preview',
  scopes: ['transaction-signing'],
  extensions: [103],
  authorization: { kind: 'software' },
  review: {
    mode: 'sign',
    approvable: true,
    items: [
      {
        index: 0,
        dependencies: [],
        conflicts: [],
        transaction: transactionReview,
      },
    ],
  },
};

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
      parseWalletApprovalRender({
        type: 'present',
        request: {
          requestId: 'request',
          walletId,
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
        request: expect.objectContaining({ requestId: 'request', walletId }),
      })
    );
    expect(() =>
      parseWalletApprovalRender({
        type: 'present',
        request: {
          requestId: 'request',
          walletId: 'AA'.repeat(20),
          kind: 'connection',
          origin: 'https://example.test',
          walletName: 'Wallet',
          networkName: 'Preview',
          scopes: ['connection'],
          extensions: [],
        },
      })
    ).toThrow('Invalid dApp consent presentation');
    expect(
      parseWalletApprovalRender({
        type: 'present',
        request: {
          requestId: 'sign',
          walletId,
          kind: 'data-sign',
          origin: 'https://example.test',
          walletName: 'Wallet',
          networkName: 'Preview',
          scopes: ['data-signing'],
          extensions: [95],
          review: {
            address: `60${'11'.repeat(28)}`,
            credentialKind: 'payment',
            payload: '4869',
            utf8Preview: 'Hi',
          },
        },
      })
    ).toEqual(
      expect.objectContaining({
        type: 'present',
        request: expect.objectContaining({
          kind: 'data-sign',
          review: expect.objectContaining({
            payload: '4869',
            utf8Preview: 'Hi',
          }),
        }),
      })
    );
    expect(
      parseWalletApprovalRender({
        type: 'present',
        request: batchPresentation,
      })
    ).toEqual(
      expect.objectContaining({
        request: expect.objectContaining({
          kind: 'batch-sign',
          review: expect.objectContaining({
            approvable: true,
            items: [expect.objectContaining({ index: 0 })],
          }),
        }),
      })
    );
    expect(() =>
      parseWalletApprovalRender({
        type: 'present',
        request: {
          ...batchPresentation,
          review: {
            ...batchPresentation.review,
            items: [
              {
                ...batchPresentation.review.items[0],
                dependencies: [
                  {
                    source: 'current-batch',
                    inputRole: 'normal',
                    outpoint: { transactionId: '33'.repeat(32), index: 0 },
                    sourceTransactionIndex: 0,
                  },
                ],
              },
            ],
          },
        },
      })
    ).toThrow('Invalid CIP-103 batch review');
    expect(() =>
      parseWalletApprovalRender({
        type: 'present',
        request: {
          requestId: 'request',
          walletId,
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

  it('validates native outcomes carried by terminal messages', () => {
    expect(
      parseWalletApprovalRender({
        type: 'terminal',
        requestId: 'native-request',
        result: {
          status: 'submission-unknown',
          transactionIds: [],
        },
      })
    ).toEqual({
      type: 'terminal',
      requestId: 'native-request',
      result: {
        status: 'submission-unknown',
        transactionIds: [],
      },
    });
    expect(() =>
      parseWalletApprovalRender({
        type: 'terminal',
        requestId: 'native-request',
        result: {
          status: 'rejected',
          errorCode: 'Raw device rejection',
        },
      })
    ).toThrow('Invalid native approval result');
    expect(
      parseWalletApprovalRender({
        type: 'terminal',
        requestId: 'sign-request',
        result: {
          status: 'signed',
          transactionIds: ['44'.repeat(32)],
        },
      })
    ).toEqual({
      type: 'terminal',
      requestId: 'sign-request',
      result: {
        status: 'signed',
        transactionIds: ['44'.repeat(32)],
      },
    });
    expect(() =>
      parseWalletApprovalRender({
        type: 'terminal',
        requestId: 'sign-request',
        result: { status: 'signed', transactionIds: [] },
      })
    ).toThrow('Invalid native approval result');
  });
});
