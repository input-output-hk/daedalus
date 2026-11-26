import fs from 'fs';
import os from 'os';
import path from 'path';
import log from 'electron-log-daedalus';
import { setupLogging } from './setupLogging';

jest.mock('electron-log-daedalus', () => ({
  __esModule: true,
  default: { transports: { console: {}, rendererConsole: {}, file: {} } },
}));

jest.mock('../config', () => ({
  get pubLogsFolderPath() {
    return (globalThis as any).mockLogPaths.pub;
  },
  get appLogsFolderPath() {
    return (globalThis as any).mockLogPaths.app;
  },
}));

jest.mock('../environment', () => ({ environment: {} }));
jest.mock('./logging', () => ({ logger: { error: jest.fn() } }));

const mockTransports = log.transports as Record<string, any>;
const mockPaths = { pub: '', app: '' };
(globalThis as any).mockLogPaths = mockPaths;

const ROTATED_APP_LOG = /^Daedalus\.json-\d{14}$/;

describe('setupLogging', () => {
  let tmpRoot: string;

  const writeLog = (name: string, content: string, mtime?: string) => {
    const filePath = path.join(mockPaths.pub, name);
    fs.writeFileSync(filePath, content);

    if (mtime) {
      const date = new Date(mtime);
      fs.utimesSync(filePath, date, date);
    }
  };
  const listFiles = () => fs.readdirSync(mockPaths.pub).sort();

  beforeEach(() => {
    tmpRoot = fs.mkdtempSync(path.join(os.tmpdir(), 'setup-logging-'));
    mockPaths.app = tmpRoot;
    mockPaths.pub = path.join(tmpRoot, 'pub');
    fs.mkdirSync(mockPaths.pub);
  });

  afterEach(() => {
    fs.rmSync(tmpRoot, { recursive: true, force: true });
  });

  it('keeps the previous session app log under a timestamped name', () => {
    writeLog('Daedalus.json', 'previous session', '2026-11-19T10:20:30.000Z');

    setupLogging();

    expect(listFiles()).toEqual(['Daedalus.json-20261119102030']);
    expect(
      fs.readFileSync(
        path.join(mockPaths.pub, 'Daedalus.json-20261119102030'),
        'utf8'
      )
    ).toBe('previous session');
  });

  it('names the kept log so that the logs download picks it up', () => {
    writeLog('Daedalus.json', 'previous session');

    setupLogging();

    expect(
      listFiles().filter((name) => ROTATED_APP_LOG.test(name))
    ).toHaveLength(1);
  });

  it('writes the new session to Daedalus.json', () => {
    writeLog('Daedalus.json', 'previous session');

    setupLogging();

    expect(mockTransports.file.file).toBe(
      path.join(mockPaths.pub, 'Daedalus.json')
    );
    expect(fs.existsSync(mockTransports.file.file)).toBe(false);
  });

  it('keeps no more app log files than the transport allows', () => {
    writeLog('Daedalus.json-20261101000000', 'a');
    writeLog('Daedalus.json-20261102000000', 'b');
    writeLog('Daedalus.json-20261103000000', 'c');
    writeLog('Daedalus.json', 'd', '2026-11-04T00:00:00.000Z');

    setupLogging();

    expect(listFiles()).toEqual([
      'Daedalus.json-20261102000000',
      'Daedalus.json-20261103000000',
      'Daedalus.json-20261104000000',
    ]);
    expect(mockTransports.file.maxItems).toBe(4);
  });

  it('does not rotate an empty app log', () => {
    writeLog('Daedalus.json', '');

    setupLogging();

    expect(listFiles()).toEqual(['Daedalus.json']);
  });

  it('leaves the other files in the folder alone', () => {
    writeLog('Daedalus.json', 'previous session');
    writeLog('Daedalus-versions.json', '{}');
    writeLog('node.log', 'node');

    setupLogging();

    expect(listFiles()).toEqual(
      expect.arrayContaining(['Daedalus-versions.json', 'node.log'])
    );
  });
});
