import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import translations from '../../../i18n/locales/en-US.json';
import { ROUTES } from '../../../routes-config';
import SettingsMenu from './SettingsMenu';

describe('settings pilot isolation', () => {
  afterEach(cleanup);

  it.each([false, true])(
    'gates connection management with capability %s',
    (enabled) => {
      global.cip30Enabled = enabled;
      const onItemClick = jest.fn();
      render(
        <IntlProvider locale="en-US" messages={translations}>
          <SettingsMenu
            isFlight={false}
            isActiveItem={() => false}
            onItemClick={onItemClick}
          />
        </IntlProvider>
      );
      const connections = screen.queryByRole('button', {
        name: 'dApp connections',
      });
      expect(connections !== null).toBe(enabled);
      if (enabled) fireEvent.click(connections!);
      expect(onItemClick.mock.calls).toEqual(
        enabled ? [[ROUTES.SETTINGS.DAPP_CONNECTIONS]] : []
      );
      fireEvent.click(screen.getByRole('button', { name: 'General' }));
      expect(onItemClick).toHaveBeenLastCalledWith(ROUTES.SETTINGS.GENERAL);
      fireEvent.click(screen.getByRole('button', { name: 'Security' }));
      expect(onItemClick).toHaveBeenLastCalledWith(ROUTES.SETTINGS.SECURITY);
    }
  );
});
