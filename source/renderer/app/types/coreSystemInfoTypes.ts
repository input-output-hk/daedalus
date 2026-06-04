export type CoreSystemInfo = {
  daedalusVersion: string;
  daedalusBuildNumber: string;
  daedalusProcessID: string;
  daedalusMainProcessID: string;
  isBlankScreenFixActive: boolean;
  cardanoNodeVersion: string;
  cardanoNodePID: number;
  cardanoNodeUptime: string;
  cardanoWalletVersion: string;
  cardanoWalletPID: number;
  cardanoWalletUptime: string;
  cardanoWalletRestartCount: number;
  cardanoWalletApiPort: number;
  cardanoNetwork: string;
  daedalusStateDirectoryPath: string;
  /** PID of the cardano-watchdog supervisor process itself */
  watchdogPid?: number;
  /** True if cardano-node had to be force-killed (SIGKILL) by the watchdog */
  nodeForceKilled?: boolean;
  /** Milliseconds the watchdog waited for the node socket to appear */
  nodeSocketWaitMs?: number;
  /** Milliseconds the watchdog waited for the wallet API to become ready */
  walletReadyWaitMs?: number;
  /** Exit code from the most recent cardano-wallet crash before the watchdog restarted it */
  lastWalletExitCode?: number | null;
};
