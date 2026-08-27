let revoke = (): void => undefined;

export const setCip30SessionRevoker = (next: () => void): void => {
  revoke = next;
};

export const revokeCip30Sessions = (): void => revoke();
