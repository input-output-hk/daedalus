// @flow
import { pick } from 'lodash';
import { observable, action } from 'mobx';
import { IS_WALLET_ASSETS_AMOUNT_FORMATTING_ENABLED } from '../config/walletsConfig';
import { DEFAULT_DECIMAL_PRECISION } from '../config/assetsConfig';
import type { AssetMetadata } from '../api/assets/types';

export type AssetProps = {
  policyId: string,
  assetName: string,
  fingerprint: string,
  metadata?: ?AssetMetadata,
  unit?: ?number,
};

export default class Asset {
  @observable policyId: string = '';
  @observable assetName: string = '';
  @observable fingerprint: string = '';
  @observable metadata: ?AssetMetadata;
  @observable unit: ?number;

  constructor(data: AssetProps) {
    let unit;
    if (IS_WALLET_ASSETS_AMOUNT_FORMATTING_ENABLED) {
      unit = data.unit || DEFAULT_DECIMAL_PRECISION;
    }
    Object.assign(this, data, { unit });
  }

  @action update(other: $Shape<AssetProps>) {
    Object.assign(
      this,
      pick(other, ['policyId', 'assetName', 'fingerprint', 'metadata', 'unit'])
    );
  }
}
