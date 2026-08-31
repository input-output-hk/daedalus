export const invokeCip95Adapter = async (
  api: {
    cip95: {
      getPubDRepKey: () => Promise<unknown>;
      getRegisteredPubStakeKeys: () => Promise<unknown>;
      getUnregisteredPubStakeKeys: () => Promise<unknown>;
      signData: (address: string, payload: string) => Promise<unknown>;
    };
  },
  drepId: string,
  type6Address: string,
  payload: string
) => {
  await api.cip95.getPubDRepKey();
  await api.cip95.getRegisteredPubStakeKeys();
  await api.cip95.getUnregisteredPubStakeKeys();
  return Promise.all([
    api.cip95.signData(drepId, payload),
    api.cip95.signData(type6Address, payload),
  ]);
};

export const captureCip103Submission = async (
  submitTxs: (transactions: string[]) => Promise<string[]>,
  transactions: string[]
): Promise<{ fulfilled?: string[]; rejected?: unknown }> => {
  try {
    return { fulfilled: await submitTxs(transactions) };
  } catch (rejected) {
    return { rejected };
  }
};
