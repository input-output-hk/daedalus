export interface MachineSpecPayload {
  cpuModel: string;
  ramBytes: number;
  os: string;
  osArch: string;
}

export interface AnalyticsClient {
  sendMachineSpec(payload: MachineSpecPayload): Promise<void>;
}
