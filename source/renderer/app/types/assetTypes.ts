/**
 * Where the pointers for on-chain asset metadata are read from.
 *
 * `koios` is the project's default instance for the network in use, `custom` is
 * an instance the user names, and `direct` derives the same pointers from the
 * chain the user already holds. There is no `none` member, which
 * `SmashServerType` has: `none` exists there for a wallet whose SMASH server is
 * unset on the server side, and this setting has no server-side counterpart to
 * be unset.
 */
export type AssetMetadataSourceType = 'koios' | 'custom' | 'direct';
