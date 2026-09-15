import { readFileSync } from 'fs';

import { readLauncherConfig } from './config';

jest.mock('fs', () => ({ readFileSync: jest.fn() }));

it('parses generated JSON launcher hardware policy', () => {
  (readFileSync as jest.Mock).mockReturnValue(
    JSON.stringify({
      dappBrowserPolicy: {
        hardwareConnectorEnabled: true,
      },
    })
  );

  expect(readLauncherConfig('/launcher-config.yaml')).toEqual({
    dappBrowserPolicy: {
      hardwareConnectorEnabled: true,
    },
  });
});
