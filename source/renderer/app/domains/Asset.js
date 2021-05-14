// @flow
import { pick } from 'lodash';
import { observable, action } from 'mobx';
import type { AssetMetadata } from '../api/assets/types';

export type AssetProps = {
  policyId: string,
  assetName: string,
  fingerprint: string,
  metadata?: ?AssetMetadata,
  decimals: ?number,
  recommendedDecimals: ?number,
};

export default class Asset {
  @observable policyId: string = '';
  @observable assetName: string = '';
  @observable uniqueId: string = '';
  @observable fingerprint: string = '';
  @observable metadata: ?AssetMetadata;
  @observable decimals: ?number;
  @observable recommendedDecimals: ?number;

  constructor(props: AssetProps) {
    const { policyId, assetName } = props;
    const uniqueId = policyId + assetName;
    Object.assign(this, props, { uniqueId });
  }

  @action update(props: $Shape<AssetProps>) {
    const { policyId, assetName } = props;
    const uniqueId = policyId + assetName;
    Object.assign(
      this,
      pick(props, [
        'policyId',
        'assetName',
        'fingerprint',
        'metadata',
        'decimals',
        'recommendedDecimals',
      ]),
      { uniqueId }
    );
  }
}
