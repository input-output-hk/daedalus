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
    disableDownloadLogs: boolean;
  }> = {}
) => {
  const handlers = {
    onRepair: jest.fn(),
    onDismiss: jest.fn(),
    onIssueClick: jest.fn(),
    onDownloadLogs: jest.fn(),
  };
  render(
    <IntlProvider locale="en-US" messages={translations}>
      <WalletDbDamageView
        view={view}
        damagedCount={props.damagedCount ?? 1}
        results={props.results ?? []}
        backupDir={props.backupDir ?? null}
        disableDownloadLogs={props.disableDownloadLogs ?? false}
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
        '4 wallets were changed by an older version of Daedalus, so Daedalus cannot open them. Your funds are safe: they are on the blockchain, not in these wallet files.'
      )
    ).toBeInTheDocument();
    expect(
      screen.getByText(
        'Daedalus can save a copy of the wallet files, then repair them. Until then, these wallets will not appear in Daedalus.'
      )
    ).toBeInTheDocument();
  });

  it('says wallet, it and this wallet file for one wallet', () => {
    renderView('offer', { damagedCount: 1 });
    expect(
      screen.getByRole('heading', { name: 'A wallet cannot be opened' })
    ).toBeInTheDocument();
    expect(
      screen.getByText(
        '1 wallet was changed by an older version of Daedalus, so Daedalus cannot open it. Your funds are safe: they are on the blockchain, not in this wallet file.'
      )
    ).toBeInTheDocument();
    expect(
      screen.getByText(
        'Daedalus can save a copy of the wallet file, then repair it. Until then, this wallet will not appear in Daedalus.'
      )
    ).toBeInTheDocument();
    expect(screen.queryByText(/these wallet/)).toBeNull();
  });

  it('repairs on Back up and repair, and leaves the wallets on Not now', () => {
    const { onRepair, onDismiss } = renderView('offer');
    fireEvent.click(screen.getByRole('button', { name: 'Back up and repair' }));
    expect(onRepair).toHaveBeenCalledTimes(1);
    fireEvent.click(screen.getByRole('button', { name: 'Not now' }));
    expect(onDismiss).toHaveBeenCalledTimes(1);
  });

  it('shows the repair running, with nothing to press', () => {
    renderView('repairing', { damagedCount: 3 });
    expect(
      screen.getByRole('heading', { name: 'Repairing wallets' })
    ).toBeInTheDocument();
    expect(
      screen.getByText(
        'Daedalus is saving a copy of the wallet files and repairing them. Please wait.'
      )
    ).toBeInTheDocument();
    expect(screen.queryByRole('button')).toBeNull();
  });

  it('says wallet, file and it while the repair runs for one wallet', () => {
    renderView('repairing', { damagedCount: 1 });
    expect(
      screen.getByRole('heading', { name: 'Repairing wallet' })
    ).toBeInTheDocument();
    expect(
      screen.getByText(
        'Daedalus is saving a copy of the wallet file and repairing it. Please wait.'
      )
    ).toBeInTheDocument();
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
      screen.getByText('1 wallet could not be repaired and is unchanged.')
    ).toBeInTheDocument();
    expect(
      screen.getByText(
        `The copy Daedalus saved before the repair is in this folder: ${BACKUP}`
      )
    ).toBeInTheDocument();
    expect(
      screen.getByText(
        'she.c.sqlite: this file differs in ways Daedalus cannot repair. The file is unchanged.'
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
      screen.getByText('4 wallets could not be repaired and are unchanged.')
    ).toBeInTheDocument();
    expect(
      screen.getByText(
        'she.c.sqlite: this file differs in ways Daedalus cannot repair. The file is unchanged.'
      )
    ).toBeInTheDocument();
    expect(
      screen.getByText(
        'she.d.sqlite: Daedalus could not save a copy. The file is unchanged.'
      )
    ).toBeInTheDocument();
    expect(
      screen.getByText(
        'she.e.sqlite: the repair did not complete. The file is unchanged.'
      )
    ).toBeInTheDocument();
    expect(
      screen.getByText(
        'she.f.sqlite: Daedalus did not repair this file. The file is unchanged.'
      )
    ).toBeInTheDocument();
    expect(
      screen.queryByText(/refused|backup_failed|repair_failed|locked/)
    ).toBeNull();
    expect(screen.queryByText(/in this folder/)).toBeNull();
  });

  it('says so when no wallet needed the repair', () => {
    renderView('done', { results: [] });
    expect(screen.getByText('No wallets needed repair.')).toBeInTheDocument();
    expect(screen.queryByText(/were repaired/)).toBeNull();
  });

  it('points to a support ticket and the logs when a wallet was not repaired', () => {
    const { onIssueClick, onDownloadLogs } = renderView('done', {
      results: [
        result('she.a.sqlite', 'repaired'),
        result('she.c.sqlite', 'refused'),
      ],
    });
    expect(
      screen.getByText(
        'For a wallet that was not repaired, download your logs and open a support ticket. The logs record the reason.'
      )
    ).toBeInTheDocument();
    fireEvent.click(
      screen.getByRole('button', { name: 'Open support ticket' })
    );
    expect(onIssueClick).toHaveBeenCalledTimes(1);
    expect(onIssueClick).toHaveBeenCalledWith(expect.stringMatching(/^https:/));
    fireEvent.click(screen.getByText('Download logs'));
    expect(onDownloadLogs).toHaveBeenCalledTimes(1);
  });

  it('does not download the logs while a download is in progress', () => {
    const { onDownloadLogs } = renderView('done', {
      results: [result('she.c.sqlite', 'refused')],
      disableDownloadLogs: true,
    });
    fireEvent.click(screen.getByText('Download logs'));
    expect(onDownloadLogs).not.toHaveBeenCalled();
  });

  it('offers no support step when every wallet was repaired or none needed repair', () => {
    renderView('done', { results: [result('she.a.sqlite', 'repaired')] });
    expect(screen.queryByText(/open a support ticket/)).toBeNull();
    expect(screen.queryByText('Open support ticket')).toBeNull();
    expect(screen.queryByText('Download logs')).toBeNull();
    cleanup();
    renderView('done', { results: [] });
    expect(screen.queryByText(/open a support ticket/)).toBeNull();
    expect(screen.queryByText('Open support ticket')).toBeNull();
    expect(screen.queryByText('Download logs')).toBeNull();
  });
});
