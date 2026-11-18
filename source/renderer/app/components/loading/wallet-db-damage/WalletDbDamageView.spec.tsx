import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../../i18n/locales/en-US.json';

import type {
  WalletDbRepairResult,
  WalletDbRepairView,
} from '../../../../../common/types/watchdog.types';
import WalletDbDamageView from './WalletDbDamageView';

const BACKUP = '/home/user/.local/share/Daedalus/mainnet/wallet-db-backups/x';

const renderView = (
  view: WalletDbRepairView,
  props: Partial<{
    damagedCount: number;
    results: Array<WalletDbRepairResult>;
    backupDir: string | null;
  }> = {}
) => {
  const handlers = { onRepair: jest.fn(), onDismiss: jest.fn() };
  render(
    <IntlProvider locale="en-US" messages={translations}>
      <WalletDbDamageView
        view={view}
        damagedCount={props.damagedCount ?? 1}
        results={props.results ?? []}
        backupDir={props.backupDir ?? null}
        {...handlers}
      />
    </IntlProvider>
  );
  return handlers;
};

const result = (
  file: string,
  outcome: WalletDbRepairResult['outcome']
): WalletDbRepairResult => ({ file, outcome, detail: '' });

describe('WalletDbDamageView', () => {
  afterEach(cleanup);

  it('offers the repair with the number of wallets and says funds are safe', () => {
    renderView('offer', { damagedCount: 4 });
    expect(
      screen.getByRole('heading', { name: 'Some wallets cannot be opened' })
    ).toBeInTheDocument();
    expect(
      screen.getByText(
        '4 wallets were changed by an older version of Daedalus, so Daedalus cannot open them. Your funds are safe. These files hold wallet data only.'
      )
    ).toBeInTheDocument();
    expect(
      screen.getByText(
        'Daedalus can save a copy of the wallet files, then repair them. Until then, these wallets do not show in Daedalus.'
      )
    ).toBeInTheDocument();
  });

  it('says wallet and it for one wallet', () => {
    renderView('offer', { damagedCount: 1 });
    expect(
      screen.getByText(
        /^1 wallet was changed by an older version of Daedalus, so Daedalus cannot open it\./
      )
    ).toBeInTheDocument();
  });

  it('repairs on Back up and repair, and leaves the wallets on Not now', () => {
    const { onRepair, onDismiss } = renderView('offer');
    fireEvent.click(screen.getByRole('button', { name: 'Back up and repair' }));
    expect(onRepair).toHaveBeenCalledTimes(1);
    fireEvent.click(screen.getByRole('button', { name: 'Not now' }));
    expect(onDismiss).toHaveBeenCalledTimes(1);
  });

  it('shows the repair running, with nothing to press', () => {
    renderView('repairing');
    expect(
      screen.getByRole('heading', { name: 'Repairing wallets' })
    ).toBeInTheDocument();
    expect(
      screen.getByText(
        'Daedalus is stopping Cardano wallet, saving a copy of the wallet files and repairing them. Then Cardano wallet starts again. Please wait.'
      )
    ).toBeInTheDocument();
    expect(screen.queryByRole('button')).toBeNull();
  });

  it('reports the results, the backup folder and why a file was not repaired', () => {
    const { onDismiss } = renderView('done', {
      results: [
        result('she.a.sqlite', 'repaired'),
        result('she.b.sqlite', 'repaired'),
        result('she.c.sqlite', 'refused'),
      ],
      backupDir: BACKUP,
    });
    expect(screen.getByText('2 wallets were repaired.')).toBeInTheDocument();
    expect(
      screen.getByText('1 wallet could not be repaired. It was not changed.')
    ).toBeInTheDocument();
    expect(
      screen.getByText(
        `A copy of the wallet files from before the repair is saved in this folder: ${BACKUP}`
      )
    ).toBeInTheDocument();
    expect(
      screen.getByText(
        'she.c.sqlite: the file has other changes, so Daedalus did not touch it.'
      )
    ).toBeInTheDocument();
    fireEvent.click(screen.getByRole('button', { name: 'Continue' }));
    expect(onDismiss).toHaveBeenCalledTimes(1);
  });

  it('says in plain words why each file was not repaired, never the outcome code', () => {
    renderView('done', {
      results: [
        result('she.c.sqlite', 'refused'),
        result('she.d.sqlite', 'backup_failed'),
        result('she.e.sqlite', 'repair_failed'),
        // An outcome a newer watchdog may send
        result(
          'she.f.sqlite',
          'locked' as unknown as WalletDbRepairResult['outcome']
        ),
      ],
      backupDir: null,
    });
    expect(
      screen.getByText(
        '4 wallets could not be repaired. They were not changed.'
      )
    ).toBeInTheDocument();
    expect(
      screen.getByText(
        'she.c.sqlite: the file has other changes, so Daedalus did not touch it.'
      )
    ).toBeInTheDocument();
    expect(
      screen.getByText(
        'she.d.sqlite: Daedalus could not save a copy, so it did not touch the file.'
      )
    ).toBeInTheDocument();
    expect(
      screen.getByText(
        'she.e.sqlite: the repair did not complete; the file is unchanged.'
      )
    ).toBeInTheDocument();
    expect(
      screen.getByText(
        'she.f.sqlite: Daedalus did not repair the file, and it is unchanged.'
      )
    ).toBeInTheDocument();
    expect(
      screen.queryByText(/refused|backup_failed|repair_failed|locked/)
    ).toBeNull();
    expect(screen.queryByText(/saved in this folder/)).toBeNull();
  });

  it('says so when no wallet needed the repair', () => {
    renderView('done', { results: [] });
    expect(
      screen.getByText('No wallet needed to be repaired.')
    ).toBeInTheDocument();
    expect(screen.queryByText(/were repaired/)).toBeNull();
  });
});
