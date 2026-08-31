import { createHash } from 'crypto';
import { readFileSync } from 'fs';
import cbor from 'cbor';
import { blake2b } from 'blakejs';
import TransportNodeHid, {
  getDevices,
} from '@ledgerhq/hw-transport-node-hid-noevents';
import AppAda, {
  AddressType,
  MessageAddressFieldType,
} from '@cardano-foundation/ledgerjs-hw-app-cardano';
import type { MessageData } from '@cardano-foundation/ledgerjs-hw-app-cardano';

import { prepareCip8Request } from '../source/common/cardano/cip8Request';
import { serializeCip8 } from '../source/common/cardano/cip8';
import { verifyHardwareTransactionWitnesses } from '../source/common/cardano/witnessSet';
import { toExactLedgerSignTransactionRequest } from '../source/common/hardware/ledgerTransaction';
import type { TransactionContextSnapshot } from '../source/common/cardano/transactionContext';
import type {
  HardwareExactTransaction,
  HardwareTransactionCapability,
} from '../source/common/types/hardware-wallets.types';
import { signCip103HardwareBatch } from '../source/main/cip30/extensions/cip103';
import { preflightCip103Sign } from '../source/common/cip30/cip103Batch';
import { prepareHardwareTransaction } from '../source/renderer/app/utils/hardwareWalletTransaction';

type Mode =
  | 'inspect'
  | 'sign-data'
  | 'sign-tx'
  | 'batch-success'
  | 'batch-reject'
  | 'batch-cancel';

type Case = Readonly<{
  id: string;
  capabilityRowId: string;
  artifactBinding: Readonly<{ id: string; version: string }>;
  modelBinding: Readonly<{ model: string; certificationVersion: number[] }>;
  fixtureBinding: Readonly<{ selectedInput: Readonly<{ sha256: string }> }>;
  inputRecipe: Readonly<{ canonicalJson: string; recipeSha256: string }>;
  certificationTarget?: Readonly<{
    inputRecipeSha256?: string;
    physicalExecution?: boolean;
  }>;
}>;

type CasesDocument = Readonly<{
  matrixRevision: string;
  cases: readonly Case[];
}>;

type Ledger = Readonly<{ transport: TransportNodeHid; app: AppAda }>;

const APPROVAL = 'LEDGER_APPROVAL_REQUIRED';
const PRODUCT_ID_NANO_X = 0x4000;
const RUNTIME_VERSION = '8.0.0';
const APP_VERSION = [7, 3, 0] as const;
const PAYMENT_PATH = [0x8000073c, 0x80000717, 0x80000000, 0, 0];
const STAKE_PATH = [0x8000073c, 0x80000717, 0x80000000, 2, 0];
const DREP_PATH = [0x8000073c, 0x80000717, 0x80000000, 3, 0];
const CASE_IDS = {
  payment: 'ledger-8-library-nano-x-app7-signdata-message-mode-payment-address',
  stake: 'ledger-8-library-nano-x-app7-signdata-message-mode-stake-address',
  drep: 'ledger-8-library-nano-x-app7-signdata-message-mode-drep-direct',
  drepType6: 'ledger-8-library-nano-x-app7-signdata-message-mode-drep-type6',
  transaction:
    'ledger-8-library-nano-x-app7-signtx-physical-transaction-single-transaction',
  batch:
    'ledger-8-library-nano-x-app7-signtx-physical-transaction-ordered-batch',
} as const;

const digest = (value: string | Buffer): string =>
  createHash('sha256').update(value).digest('hex');

const fail = (): never => {
  throw new Error('task607-failed');
};

const assert: (value: unknown) => asserts value = (value) => {
  if (!value) fail();
};

const emit = (value: Record<string, unknown>): void => {
  process.stdout.write(`${JSON.stringify(value)}\n`);
};

const parseArgs = (): Readonly<{
  mode: Mode;
  operator: string;
  index?: 0 | 1 | 2;
}> => {
  const [, , mode, ...rest] = process.argv;
  assert(
    mode === 'inspect' ||
      mode === 'sign-data' ||
      mode === 'sign-tx' ||
      mode === 'batch-success' ||
      mode === 'batch-reject' ||
      mode === 'batch-cancel'
  );
  const args = new Map<string, string>();
  for (let index = 0; index < rest.length; index += 2) {
    const key = rest[index];
    const value = rest[index + 1];
    assert(key?.startsWith('--') && value !== undefined && !args.has(key));
    args.set(key, value);
  }
  const operator = args.get('--operator');
  assert(operator && /^[a-z0-9][a-z0-9-]{2,63}$/u.test(operator));
  const indexValue = args.get('--index');
  assert(args.size === 1 || (args.size === 2 && indexValue !== undefined));
  if (mode === 'batch-reject')
    assert(indexValue === '0' || indexValue === '1' || indexValue === '2');
  else assert(indexValue === undefined);
  return {
    mode,
    operator,
    ...(indexValue === undefined
      ? {}
      : { index: Number(indexValue) as 0 | 1 | 2 }),
  };
};

const casesDocument = (): CasesDocument =>
  JSON.parse(
    readFileSync('hardware-wallet-tests/capability-matrix/cases.json', 'utf8')
  ) as CasesDocument;

const selectedCase = (document: CasesDocument, id: string): Case => {
  const selected = document.cases.filter(
    (candidate) =>
      candidate.id === id &&
      candidate.capabilityRowId === 'ledger-8-library-nano-x-app7' &&
      candidate.modelBinding.model === 'ledger-nano-x' &&
      candidate.artifactBinding.version === RUNTIME_VERSION
  );
  assert(selected.length === 1);
  const result = selected[0];
  assert(
    result.certificationTarget?.physicalExecution === true &&
      result.certificationTarget.inputRecipeSha256 ===
        result.inputRecipe.recipeSha256
  );
  assert(
    result.modelBinding.certificationVersion.length === 3 &&
      result.modelBinding.certificationVersion[0] === APP_VERSION[0]
  );
  assert(
    result.modelBinding.certificationVersion[1] < APP_VERSION[1] ||
      (result.modelBinding.certificationVersion[1] === APP_VERSION[1] &&
        result.modelBinding.certificationVersion[2] <= APP_VERSION[2])
  );
  return result;
};

const approval = async <T>(execute: () => Promise<T>): Promise<T> => {
  process.stdout.write(`${APPROVAL}\n`);
  return execute();
};

const openLedger = async (): Promise<Ledger> => {
  const devices = getDevices().filter(
    (device) => device.productId === PRODUCT_ID_NANO_X
  );
  assert(devices.length === 1);
  const transport = await approval(() =>
    TransportNodeHid.open(devices[0].path)
  );
  return { transport, app: new AppAda(transport) };
};

const checkVersion = async (ledger: Ledger): Promise<string> => {
  const { version } = await approval(() => ledger.app.getVersion());
  assert(
    version.major === APP_VERSION[0] &&
      version.minor === APP_VERSION[1] &&
      version.patch === APP_VERSION[2]
  );
  return `${version.major}.${version.minor}.${version.patch}`;
};

const key = async (
  ledger: Ledger,
  path: readonly number[]
): Promise<Buffer> => {
  const result = await approval(() =>
    ledger.app.getExtendedPublicKey({ path: [...path] })
  );
  assert(/^[0-9a-f]{64}$/u.test(result.publicKeyHex));
  return Buffer.from(result.publicKeyHex, 'hex');
};

const keyHash = (publicKey: Buffer): string =>
  Buffer.from(blake2b(publicKey, undefined, 28)).toString('hex');

const capability = (
  document: CasesDocument,
  selected: Case
): HardwareTransactionCapability => ({
  matrixRevision: document.matrixRevision,
  artifactId: selected.artifactBinding.id,
  rowId: selected.capabilityRowId,
  vendor: 'ledger',
  staticallyRepresentable: true,
  staticGatesPassed: true,
  physicalCertified: true,
  // Certification harness only; production activation remains disabled.
  productEnabled: true,
  familyDispositions: { 'root-envelope': 'representable' },
});

const ordinaryCbor = (
  transactionIdByte: number,
  credential: string
): string => {
  const address = Buffer.from(`60${credential}`, 'hex');
  const body = new Map<number, unknown>([
    [0, [[Buffer.alloc(32, transactionIdByte), 0]]],
    [1, [[address, 900_000]]],
    [2, 100_000],
  ]);
  return cbor.encodeCanonical([body, new Map(), true, null]).toString('hex');
};

const snapshot = (
  cbors: readonly string[],
  credential: string,
  requiredIndexes: readonly number[]
): TransactionContextSnapshot => {
  const preflight = preflightCip103Sign(
    cbors.map((cborHex) => ({ cbor: cborHex, partialSign: true })),
    0
  );
  const output = (transactionId: string, index: number) => {
    const sourceCbor = cbor
      .encodeCanonical([Buffer.from(`60${credential}`, 'hex'), 1_000_000])
      .toString('hex');
    return {
      outpoint: { transactionId, index: 0 },
      sourceCbor,
      inputCbor: '',
      canonicalCbor: sourceCbor,
      unspentCbor: '',
      provenance: ['node'] as const,
      roles: ['normal'] as const,
      walletMember: true,
      pendingState: 'none' as const,
    };
  };
  return {
    walletId: '00'.repeat(20),
    network: { networkId: 0, networkMagic: 42, genesisHash: '00'.repeat(32) },
    chainPoint: { kind: 'genesis' },
    walletGeneration: BigInt(1),
    pendingGeneration: BigInt(1),
    contextDigest: '00'.repeat(32),
    contextToken: '00'.repeat(32),
    records: [],
    transactions: [...cbors],
    outputs: requiredIndexes.map((index) =>
      output(
        preflight.items[index].transaction.inputs.normal[0].transactionId,
        index
      )
    ),
    pendingTransactions: [],
    ownership: [
      {
        credentialKind: 'payment',
        credential,
        ownership: 'owned_key',
        derivationPath: PAYMENT_PATH,
        proofKinds: ['normal_input'],
      },
    ],
    requiredProofs: requiredIndexes.map((transactionIndex) => ({
      transactionIndex,
      proofKind: 'normal_input',
      credentialKind: 'payment',
      credential,
      required: true,
    })),
    commitmentContexts: [],
    transactionsSemantic: preflight.items.map((item) => item.transaction),
    preExistingWitnesses: [],
  };
};

type TransactionProof = Readonly<{
  witnessSet: string;
  returnedBodyHashDigest: string;
  returnedPublicKeyDigest: string;
  returnedSignatureDigest: string;
  witnessCount: number;
}>;

const signExact = async (
  ledger: Ledger,
  exact: HardwareExactTransaction
): Promise<TransactionProof> => {
  const request = toExactLedgerSignTransactionRequest(exact);
  let signed: Awaited<ReturnType<AppAda['signTransaction']>>;
  try {
    signed = await approval(() => ledger.app.signTransaction(request));
  } catch (error) {
    if (
      error &&
      typeof error === 'object' &&
      'code' in error &&
      (error.code === 0x6e09 || error.code === 0x6985)
    )
      throw Object.assign(new Error('declined'), {
        code: 'TxSignError.UserDeclined',
      });
    throw error;
  }
  assert(signed.txHashHex === exact.bodyHash);
  const expected = exact.signers.map(({ path, keyHash: expectedHash }) => ({
    path: path.join('/'),
    keyHash: expectedHash,
  }));
  const witnesses: Array<{ publicKey: string; signature: string }> = [];
  for (const witness of signed.witnesses) {
    const expectedWitness = expected.find(
      (candidate) => candidate.path === witness.path.join('/')
    );
    assert(expectedWitness);
    const publicKey = await key(ledger, witness.path);
    assert(keyHash(publicKey) === expectedWitness.keyHash);
    witnesses.push({
      publicKey: publicKey.toString('hex'),
      signature: witness.witnessSignatureHex,
    });
  }
  assert(witnesses.length === expected.length);
  return {
    witnessSet: verifyHardwareTransactionWitnesses(exact, {
      bodyHash: signed.txHashHex,
      witnesses,
    }),
    returnedBodyHashDigest: digest(Buffer.from(signed.txHashHex, 'hex')),
    returnedPublicKeyDigest: digest(
      Buffer.concat(
        witnesses.map(({ publicKey }) => Buffer.from(publicKey, 'hex'))
      )
    ),
    returnedSignatureDigest: digest(
      Buffer.concat(
        witnesses.map(({ signature }) => Buffer.from(signature, 'hex'))
      )
    ),
    witnessCount: witnesses.length,
  };
};

const messageRecipe = (
  selected: Case,
  messageMode: string,
  credentialKind: 'address' | 'key-hash',
  hasBoundCredential: boolean
): Readonly<{ payloadSha256: string }> => {
  const recipe = JSON.parse(selected.inputRecipe.canonicalJson) as {
    operation?: unknown;
    messageMode?: unknown;
    payloadSha256?: unknown;
    request?: Readonly<{
      method?: unknown;
      credentialKind?: unknown;
      hasBoundCredential?: unknown;
      hashPayload?: unknown;
    }>;
  };
  assert(
    recipe.operation === 'signData' &&
      recipe.messageMode === messageMode &&
      typeof recipe.payloadSha256 === 'string' &&
      recipe.request?.method === 'signMessage' &&
      recipe.request.credentialKind === credentialKind &&
      recipe.request.hasBoundCredential === hasBoundCredential &&
      recipe.request.hashPayload === false
  );
  return { payloadSha256: recipe.payloadSha256 };
};

const messagePayload = (): string => {
  const fixture = JSON.parse(
    readFileSync(
      'source/common/cip30/contracts/fixtures/cip8-cip95-fixture.json',
      'utf8'
    )
  ) as { payload?: unknown };
  assert(
    typeof fixture.payload === 'string' && /^[0-9a-f]+$/u.test(fixture.payload)
  );
  return fixture.payload;
};

const transactionRecipe = (
  selected: Case,
  execution: 'single-transaction' | 'ordered-batch'
): void => {
  const recipe = JSON.parse(selected.inputRecipe.canonicalJson) as {
    operation?: unknown;
    contextFixture?: unknown;
    execution?: unknown;
    verification?: unknown;
    itemKinds?: unknown;
    signedIndices?: unknown;
    release?: unknown;
    refusalIndices?: unknown;
  };
  assert(
    recipe.operation === 'signTx' &&
      recipe.contextFixture === 'task-607-ordinary-ledger-context-v1' &&
      recipe.execution === execution &&
      recipe.verification === 'immutable-body-hash-and-witnesses'
  );
  if (execution === 'ordered-batch') {
    assert(
      Array.isArray(recipe.itemKinds) &&
        recipe.itemKinds.join('/') ===
          'ready/canonical-empty-witness-set/ready' &&
        Array.isArray(recipe.signedIndices) &&
        recipe.signedIndices.join('/') === '0/2' &&
        recipe.release === 'all-after-success' &&
        Array.isArray(recipe.refusalIndices) &&
        recipe.refusalIndices.join('/') === '0/1/2'
    );
  }
};

type MessageProof = Readonly<{
  caseId: string;
  inputDigest: string;
  inputRecipeSha256: string;
  proof: Readonly<{
    identityVerified: true;
    publicKeyAssociated: true;
    signatureVerified: true;
    localCoseVerified: true;
    vendorCosePassedThrough: false;
    returnedPublicKeyDigest: string;
    returnedSignatureDigest: string;
  }>;
}>;

const signMessage = async (
  ledger: Ledger,
  selected: Case,
  kind: 'payment' | 'stake' | 'drep' | 'drepType6'
): Promise<MessageProof> => {
  const messageMode =
    kind === 'drepType6'
      ? 'drep-type6'
      : kind === 'drep'
      ? 'drep-direct'
      : `${kind}-address`;
  const isAddress = kind === 'payment' || kind === 'stake';
  const recipe = messageRecipe(
    selected,
    messageMode,
    isAddress ? 'address' : 'key-hash',
    isAddress
  );
  const payload = messagePayload();
  assert(digest(Buffer.from(payload, 'hex')) === recipe.payloadSha256);
  const payment =
    kind === 'payment' ? await key(ledger, PAYMENT_PATH) : undefined;
  const stake =
    kind === 'payment' || kind === 'stake'
      ? await key(ledger, STAKE_PATH)
      : undefined;
  const drep =
    kind === 'drep' || kind === 'drepType6'
      ? await key(ledger, DREP_PATH)
      : undefined;
  const expected =
    kind === 'payment'
      ? prepareCip8Request(
          `01${keyHash(payment!)}${keyHash(stake!)}`,
          payload,
          {
            networkId: 1,
          }
        )
      : kind === 'stake'
      ? prepareCip8Request(`e1${keyHash(stake!)}`, payload, { networkId: 1 })
      : kind === 'drep'
      ? prepareCip8Request(keyHash(drep!), payload, {
          networkId: 1,
          drepCredential: keyHash(drep!),
        })
      : prepareCip8Request(`61${keyHash(drep!)}`, payload, {
          networkId: 1,
          drepCredential: keyHash(drep!),
        });
  const messageRequest = (kind === 'payment'
    ? {
        messageHex: payload,
        signingPath: PAYMENT_PATH,
        hashPayload: false,
        preferHexDisplay: false,
        addressFieldType: MessageAddressFieldType.ADDRESS,
        address: {
          type: AddressType.BASE_PAYMENT_KEY_STAKE_KEY,
          params: { spendingPath: PAYMENT_PATH, stakingPath: STAKE_PATH },
        },
        network: { networkId: 1, protocolMagic: 764824073 },
      }
    : kind === 'stake'
    ? {
        messageHex: payload,
        signingPath: STAKE_PATH,
        hashPayload: false,
        preferHexDisplay: false,
        addressFieldType: MessageAddressFieldType.ADDRESS,
        address: {
          type: AddressType.REWARD_KEY,
          params: { stakingPath: STAKE_PATH },
        },
        network: { networkId: 1, protocolMagic: 764824073 },
      }
    : {
        messageHex: payload,
        signingPath: DREP_PATH,
        hashPayload: false,
        preferHexDisplay: false,
        addressFieldType: MessageAddressFieldType.KEY_HASH,
      }) as MessageData;
  const signed = await approval(() => ledger.app.signMessage(messageRequest));
  assert(/^[0-9a-f]{64}$/u.test(signed.signingPublicKeyHex));
  assert(/^[0-9a-f]{128}$/u.test(signed.signatureHex));
  assert(
    signed.addressFieldHex ===
      (isAddress ? expected.address : expected.credential.toString('hex'))
  );
  serializeCip8(expected, {
    publicKey: Buffer.from(signed.signingPublicKeyHex, 'hex'),
    signature: Buffer.from(signed.signatureHex, 'hex'),
  });
  return {
    caseId: selected.id,
    inputDigest: selected.fixtureBinding.selectedInput.sha256,
    inputRecipeSha256: selected.inputRecipe.recipeSha256,
    proof: {
      identityVerified: true,
      publicKeyAssociated: true,
      signatureVerified: true,
      localCoseVerified: true,
      vendorCosePassedThrough: false,
      returnedPublicKeyDigest: digest(
        Buffer.from(signed.signingPublicKeyHex, 'hex')
      ),
      returnedSignatureDigest: digest(Buffer.from(signed.signatureHex, 'hex')),
    },
  };
};

const runMessageCases = async (
  ledger: Ledger,
  document: CasesDocument,
  operatorDigest: string,
  observedRuntimeVersion: string
): Promise<void> => {
  const requests = [
    [selectedCase(document, CASE_IDS.payment), 'payment'],
    [selectedCase(document, CASE_IDS.stake), 'stake'],
    [selectedCase(document, CASE_IDS.drep), 'drep'],
    [selectedCase(document, CASE_IDS.drepType6), 'drepType6'],
  ] as const;
  const drep = messageRecipe(requests[2][0], 'drep-direct', 'key-hash', false);
  const drepType6 = messageRecipe(
    requests[3][0],
    'drep-type6',
    'key-hash',
    false
  );
  assert(drep.payloadSha256 === drepType6.payloadSha256);
  const proofs: MessageProof[] = [];
  for (const [selected, kind] of requests) {
    proofs.push(await signMessage(ledger, selected, kind));
  }
  emit({
    mode: 'sign-data',
    ok: true,
    operatorDigest,
    observedRuntimeVersion,
    caseProofs: proofs,
    drepNormalized:
      proofs[2].proof.returnedPublicKeyDigest ===
        proofs[3].proof.returnedPublicKeyDigest &&
      proofs[2].proof.returnedSignatureDigest ===
        proofs[3].proof.returnedSignatureDigest,
  });
};

const runTransaction = async (
  ledger: Ledger,
  document: CasesDocument,
  operatorDigest: string,
  observedRuntimeVersion: string
): Promise<void> => {
  const selected = selectedCase(document, CASE_IDS.transaction);
  const credential = keyHash(await key(ledger, PAYMENT_PATH));
  transactionRecipe(selected, 'single-transaction');
  const cborHex = ordinaryCbor(0x11, credential);
  const prepared = prepareHardwareTransaction(
    snapshot([cborHex], credential, [0]),
    0,
    false,
    capability(document, selected)
  );
  assert(prepared.status === 'ready');
  const transactionProof = await signExact(ledger, prepared.exact);
  emit({
    mode: 'sign-tx',
    ok: true,
    operatorDigest,
    observedRuntimeVersion,
    caseId: selected.id,
    inputDigest: selected.fixtureBinding.selectedInput.sha256,
    inputRecipeSha256: selected.inputRecipe.recipeSha256,
    proof: {
      bodyHashVerified: true,
      publicKeyAssociated: true,
      signatureVerified: true,
      returnedBodyHashDigest: transactionProof.returnedBodyHashDigest,
      returnedPublicKeyDigest: transactionProof.returnedPublicKeyDigest,
      returnedSignatureDigest: transactionProof.returnedSignatureDigest,
      witnessCount: transactionProof.witnessCount,
    },
    witnessSetDigest: digest(transactionProof.witnessSet),
  });
};

const runBatch = async (
  ledger: Ledger,
  document: CasesDocument,
  operatorDigest: string,
  observedRuntimeVersion: string,
  mode: 'batch-success' | 'batch-reject' | 'batch-cancel',
  rejectedIndex?: 0 | 1 | 2
): Promise<void> => {
  const selected = selectedCase(document, CASE_IDS.batch);
  const credential = keyHash(await key(ledger, PAYMENT_PATH));
  transactionRecipe(selected, 'ordered-batch');
  const cbors = [
    ordinaryCbor(0x11, credential),
    ordinaryCbor(0x22, credential),
    ordinaryCbor(0x33, credential),
  ];
  const requiredIndexes =
    mode === 'batch-success' || mode === 'batch-cancel' ? [0, 2] : [0, 1, 2];
  const transactionSnapshot = snapshot(cbors, credential, requiredIndexes);
  const batch = preflightCip103Sign(
    cbors.map((cbor) => ({ cbor, partialSign: true })),
    0
  );
  const review = {
    mode: 'sign' as const,
    approvable: true,
    items: batch.items.map((item, index) => ({
      index,
      transaction: {
        transactionId: item.bodyHash,
        fullCborDigest: item.fullCborDigest,
        fullCbor: item.cbor,
      },
    })),
  };
  const controller = new AbortController();
  if (mode === 'batch-cancel') process.once('SIGINT', () => controller.abort());
  let released = false;
  const transactionProofs: TransactionProof[] = [];
  try {
    const result = await signCip103HardwareBatch({
      batch,
      review,
      signal: controller.signal,
      prepare: (index) =>
        prepareHardwareTransaction(
          transactionSnapshot,
          index,
          true,
          capability(document, selected)
        ),
      signTransaction: async (exact) => {
        const proof = await signExact(ledger, exact);
        transactionProofs.push(proof);
        return proof.witnessSet;
      },
      cancelDevice: () => ledger.transport.close(),
    });
    released = true;
    if (mode !== 'batch-success') fail();
    assert(
      result.length === 3 &&
        result[0] !== 'a0' &&
        result[1] === 'a0' &&
        result[2] !== 'a0'
    );
    emit({
      mode,
      ok: true,
      operatorDigest,
      observedRuntimeVersion,
      caseId: selected.id,
      inputDigest: selected.fixtureBinding.selectedInput.sha256,
      inputRecipeSha256: selected.inputRecipe.recipeSha256,
      ordered: true,
      canonicalA0: true,
      proof: {
        bodyHashVerified: true,
        publicKeyAssociated: true,
        signatureVerified: true,
        returnedBodyHashDigest: digest(
          Buffer.concat(
            transactionProofs.map(({ returnedBodyHashDigest }) =>
              Buffer.from(returnedBodyHashDigest, 'hex')
            )
          )
        ),
        returnedPublicKeyDigest: digest(
          Buffer.concat(
            transactionProofs.map(({ returnedPublicKeyDigest }) =>
              Buffer.from(returnedPublicKeyDigest, 'hex')
            )
          )
        ),
        returnedSignatureDigest: digest(
          Buffer.concat(
            transactionProofs.map(({ returnedSignatureDigest }) =>
              Buffer.from(returnedSignatureDigest, 'hex')
            )
          )
        ),
        witnessCount: transactionProofs.reduce(
          (total, { witnessCount }) => total + witnessCount,
          0
        ),
      },
      itemBodyHashDigests: transactionProofs.map(
        ({ returnedBodyHashDigest }) => returnedBodyHashDigest
      ),
      witnessSetDigests: result.map(digest),
    });
  } catch (error) {
    if (mode === 'batch-success') throw error;
    const failure =
      error && typeof error === 'object' && 'failure' in error
        ? error.failure
        : undefined;
    const transactionIndex =
      error &&
      typeof error === 'object' &&
      'transactionIndex' in error &&
      typeof error.transactionIndex === 'number'
        ? error.transactionIndex
        : undefined;
    assert(
      (mode === 'batch-reject' &&
        failure === 'user-declined' &&
        transactionIndex === rejectedIndex) ||
        (mode === 'batch-cancel' && failure === 'cancelled')
    );
    emit({
      mode,
      ok: true,
      operatorDigest,
      observedRuntimeVersion,
      released,
      rejectedIndex: mode === 'batch-reject' ? rejectedIndex : undefined,
      cancelled: mode === 'batch-cancel',
      physicalDisconnectRequired: mode === 'batch-cancel',
    });
  }
};

const run = async (): Promise<void> => {
  const args = parseArgs();
  const operatorDigest = digest(args.operator);
  const document = casesDocument();
  assert(document.matrixRevision === 'task-006-matrix-2026-08-14');
  const ledgerPackage = require('@cardano-foundation/ledgerjs-hw-app-cardano/package.json') as {
    version?: unknown;
  };
  assert(ledgerPackage.version === RUNTIME_VERSION);
  const ledger = await openLedger();
  try {
    const observedRuntimeVersion = await checkVersion(ledger);
    if (args.mode === 'inspect') {
      emit({
        mode: args.mode,
        ok: true,
        productIdentityVerified: true,
        observedRuntimeVersion,
        runtimeVersion: RUNTIME_VERSION,
        operatorDigest,
      });
    } else if (args.mode === 'sign-data') {
      await runMessageCases(
        ledger,
        document,
        operatorDigest,
        observedRuntimeVersion
      );
    } else if (args.mode === 'sign-tx') {
      await runTransaction(
        ledger,
        document,
        operatorDigest,
        observedRuntimeVersion
      );
    } else {
      await runBatch(
        ledger,
        document,
        operatorDigest,
        observedRuntimeVersion,
        args.mode,
        args.index
      );
    }
  } finally {
    await ledger.transport.close().catch(() => undefined);
  }
};

run().catch(() => {
  emit({ ok: false, failure: 'task607-failed' });
  process.exitCode = 1;
});
