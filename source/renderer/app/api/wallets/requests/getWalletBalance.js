// @flow
import type { RequestConfig } from '../../common/types';
import type { GetWalletBalanceResponse } from '../types';
import { request } from '../../utils/request';

export const getWalletBalance = (
  config: RequestConfig,
  rawSecret: string, // EncryptedSecretKey
): Promise<GetWalletBalanceResponse> => (
  request({
    method: 'POST',
    path: '/api/internal/query-balance',
    ...config,
  }, {}, rawSecret)
);

/* eslint-disable max-len */
/*

[clever@system76:~/iohk/cardano-bittrex/wallet/states/testnet]$ time curl --cert tls/client/client.pem --cacert tls/server/ca.crt https://localhost:8090/api/internal/query-balance -X POST -d '"825880d3c1df2f6d618d1f7263ff1ead5808a9f40db72172d93933c21fcfff5e47843f9a4edcb93ad0ca5e0cba936d44d4082863e890ab5b5ba73834b5457ce61bdd6509a69c15546350d399dbfcee1a071b28ebdc290e809b8f5207af86ec16c43aa5be1d4f5bb91da36dd9f5f3081f6b047f0719a23a27f3428aea66afdf8c04fed3586031347c387c317c78374f6936465a79716830424467774f48676456367534395050747141314c5950307751447a4d7634526b3d7c5038533858316f6f5154394f594a6f446e786b2f7877545a62564251734d593174414d5666773776616e633d"' -H "Content-Type: application/json; charset=utf-8" -v
{"data":{"balance":2725394944,"walletId":"2cWKMJemoBamKb8mbQxL4we5QPZzuzp1QbsVFFEAnstGN4yMaaWz5bVyoD14EKDizvd8c"},"status":"success","meta":{"pagination":{"totalPages":1,"page":1,"perPage":1,"totalEntries":1}}}

*/
/* eslint-disable max-len */
