/// <reference types="node" />
export declare type EncryptedSecretKey = {
    encryptedPayload: any;
    passphraseHash: Buffer;
    isEmptyPassphrase: Boolean;
    walletId: WalletId;
};
export declare type WalletId = String;
export declare function decodeKeystore(bytes: Buffer): Promise<[EncryptedSecretKey]>;
export declare function prettyKeystore(keystore: [EncryptedSecretKey]): String;
//# sourceMappingURL=index.d.ts.map