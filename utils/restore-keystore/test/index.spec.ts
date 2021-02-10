import * as fs from 'fs';
import * as path from 'path';
import { decodeKeystore } from '../src';

let keystore;
beforeAll(async () => {
  const bytes = fs.readFileSync(path.join(__dirname, "secrets", "secret.key"));
  keystore = await decodeKeystore(bytes);
});

describe('walletId', () => {
  test('0', () => {
    expect(keystore[0].walletId)
      .toBe('5c409c7e4fddfa99d11c0345dc75e514d2a0faad');
  });

  test('1', () => {
    expect(keystore[1].walletId)
      .toBe('05377e480bd68a2da04f00c39ce5886e4746ed1f');
  });

  test('2', () => {
    expect(keystore[2].walletId)
      .toBe('781647fe8876d7e09e3d2da23658482deebfb0d1');
  });

  test('3', () => {
    expect(keystore[3].walletId)
      .toBe('0a436b1142929758a54f6008ff558ff8fa264e76');
  });
});

describe('encryptedPayload', () => {
  test('0', () => {
    expect(keystore[0].encryptedPayload.toString('hex'))
      .toBe('f6e79f49b8999a39d7e970e42d0a91224ecacefc3aa1edb342f34eb8bc6c2fc6'+
            '3e743b862b312a6f92ba0161d4d53c3ee5a2bd8085476d9575765c49dceecbe5'+
            '4b34ec47daf9b7ebc6bdb706622616451c000e85ba81c7449ae436a8cbbf3aab'+
            '98e5cc704977bd11bb0ba8d5b5571a705704cb9334d27a048532eab49a698c2d');
  });

  test('1', () => {
    expect(keystore[1].encryptedPayload.toString('hex'))
      .toBe('38e8de9c583441213fe34eecc4e28265267466877ba4048e3ab1fa9956366947'+
            'aefaf5ba9779db67eead7fc9cd1354b994a5d8d9cd40ab874bfeb1b33649280c'+
            'd33651377731e0e59e0233425a55257782c5adaa768da0567f43c1c6c0c18766'+
            'ed0a547bb34eb472c120b170a8640279832ddf18002887f03c15dea59705422d');
  });

  test('2', () => {
    expect(keystore[2].encryptedPayload.toString('hex'))
      .toBe('b824f5268bdf05d783b0c594936e51c65d6d155021ddcbdb3370bb7a51caba4b'+
            'edece046fc9d12143ce900d8238e444904e08a85af80299b012e7f12030a2ab6'+
            'd3d1cf6fc565476c17326baebddcf31c5e3409ba697b9bf04ac588cb59868c22'+
            'd043e0237a3ba2eb167d1504dd93f788fc4a72503a261cf338ad7e4cf8837aff');
  });

  test('3', () => {
    expect(keystore[3].encryptedPayload.toString('hex'))
      .toBe('5da58210e3f6e2b1e24659158554f91bb5970202ba0e0cd2d32767ce3c2893c2'+
            '9515a5469493f6133857c0dc86f127aa70e46d047424e07f7221715357bfb350'+
            '52aed22cefd53ea7d75b5804add54e653c21e03ffd7c01cb8f6c033f7d383cc1'+
            '27046148d89a1828b76bdb46b56f6fec06d31b5b412dc68ab4b5ffa2418e7e8d');
  });
});

describe('passphraseHash', () => {
  test('0', () => {
    expect(keystore[0].passphraseHash.toString('hex'))
      .toBe('31347c387c317c5743413633702f6a487a5777575278756756344e3168547934'+
            '70646c6d4f76665177653863775a575472784f79773d3d7c796341722f61326f'+
            '4f777a736e4e746f4e655049416e4f6b7978426549494a6b59623039574b564a'+
            '7159493d');
  });

  test('1', () => {
    expect(keystore[1].passphraseHash.toString('hex'))
      .toBe('31347c387c317c574342652b796362417576356c2b4258676a344a314c634367'+
            '5375414c2f5653393661364e576a2b7550766655513d3d7c6f78463665493973'+
            '4151444e6f38395147747366324e653937426338372b484b6b4137756772752f'+
            '5970673d');
  });

  test('2', () => {
    expect(keystore[2].passphraseHash.toString('hex'))
      .toBe('31347c387c317c5743436e774771435944796e4233684d477168706234556c64'+
            '392b6a6844506b3836314e5a773865382b637954673d3d7c524e53642f35537a'+
            '51704857593148375161594b3466423045466f765659564830666b2b4d6c5345'+
            '4d64733d');
  });

  test('3', () => {
    expect(keystore[3].passphraseHash.toString('hex'))
      .toBe('31347c387c317c57434478796c7064635a736853367a6d3150766d54456a3466'+
            '7038795a34623145557868627765347a36596645413d3d7c48544739445a456d'+
            '4f2b79535a6a705a76553145614d795a437170732b4334733070473841456274'+
            '5638773d');
  });
});

describe('isEmptyPassphrase', () => {
  test('0', () => {
    expect(keystore[0].isEmptyPassphrase).toBe(false);
  });

  test('1', () => {
    expect(keystore[1].isEmptyPassphrase).toBe(true);
  });

  test('2', () => {
    expect(keystore[2].isEmptyPassphrase).toBe(true);
  });

  test('3', () => {
    expect(keystore[3].isEmptyPassphrase).toBe(false);
  });
});
