import { EventMessage, validMessage } from '../../common/analytics/contract';
import {
  analyticsCpus,
  analyticsNetworks,
} from '../../common/analytics/vocabulary';

export type Device = {
  platform: string;
  osVersion: string;
  ram: number;
  cpu: string;
  appVersion: string;
  network: string;
};

export function normalizeEvent(
  input: unknown,
  id: string,
  device: Device,
  now: number,
  attemptId?: string
) {
  if (
    !validMessage(input, now) ||
    !/^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/.test(
      id
    )
  )
    return null;
  if (
    input.type === 'funnel_step' &&
    !/^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/.test(
      attemptId || ''
    )
  )
    return null;
  const os = { win32: 'Windows', darwin: 'macOS', linux: 'Linux' }[
    device.platform
  ];
  const ram = Math.ceil(device.ram / 1024 ** 3);
  if (
    !os ||
    !Number.isInteger(ram) ||
    ram < 1 ||
    ram > 2048 ||
    !/^\d{1,3}\.\d{1,3}\.\d{1,3}$/.test(device.appVersion) ||
    !(analyticsNetworks as readonly string[]).includes(device.network)
  )
    return null;
  const cpu = (analyticsCpus as readonly string[]).includes(device.cpu)
    ? device.cpu
    : 'Other';
  return {
    version: input.type === 'funnel_step' ? 2 : 1,
    user_id: id,
    site_id: device.network,
    type: input.type,
    action: input.action,
    ...(input.type === 'funnel_step'
      ? {
          attempt_id: attemptId,
          sequence: input.stage === 'started' ? 0 : 1,
          stage: input.stage,
        }
      : {}),
    ...(input.category ? { category: input.category } : {}),
    ...(input.label ? { label: input.label } : {}),
    dimensions: {
      os,
      os_version: /^\d{1,5}(\.\d{1,5}){0,3}$/.test(device.osVersion)
        ? device.osVersion
        : null,
      ram_gb: ram,
      cpu,
      app_version: device.appVersion,
      uses_legacy_wallet: input.uses_legacy_wallet,
      uses_hardware_wallet: input.uses_hardware_wallet,
    },
    ts: (input as EventMessage).ts,
  };
}
