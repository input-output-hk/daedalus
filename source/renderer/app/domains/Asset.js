// @flow
import { pick } from 'lodash';
import { observable, action } from 'mobx';
import type { AssetMetadata } from '../api/assets/types';

export type AssetDomainProps = {
  assetName: string,
  decimals: ?number,
  fingerprint: string,
  metadata?: ?AssetMetadata,
  policyId: string,
  recommendedDecimals: ?number,
  uniqueId: string,
};

export default class Asset {
  @observable policyId: string = '';
  @observable assetName: string = '';
  @observable uniqueId: string = '';
  @observable fingerprint: string = '';
  @observable metadata: ?AssetMetadata;
  @observable decimals: ?number;
  @observable recommendedDecimals: ?number;

  constructor(props: AssetDomainProps) {
    const { policyId, assetName } = props;
    const uniqueId = policyId + assetName;
    Object.assign(this, props, { uniqueId });
  }

  @action update(props: $Shape<AssetDomainProps>) {
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
