import { readFileSync } from 'fs';

import { readLauncherConfig } from './config';

jest.mock('fs', () => ({ readFileSync: jest.fn() }));

it('parses generated JSON launcher policies containing connector row IDs', () => {
  (readFileSync as jest.Mock).mockReturnValue(
    JSON.stringify({
      dappBrowserPolicy: {
        hardwareConnectorRows: ['ledger:europa:7.3.1:signData'],
      },
    })
  );

  expect(readLauncherConfig('/launcher-config.yaml')).toEqual({
    dappBrowserPolicy: {
      hardwareConnectorRows: ['ledger:europa:7.3.1:signData'],
    },
  });
});
