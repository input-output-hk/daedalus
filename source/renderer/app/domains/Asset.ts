import { pick } from 'lodash';
import { observable, action } from 'mobx';
import type { Asset as AssetProps, AssetMetadata } from '../api/assets/types';
export default class Asset {
  @observable
  policyId: string = '';
  @observable
  assetName: string = '';
  @observable
  uniqueId: string = '';
  @observable
  fingerprint: string = '';
  @observable
  metadata: AssetMetadata | null | undefined;
  @observable
  decimals: number | null | undefined;
  @observable
  recommendedDecimals: number | null | undefined;

  constructor(props: AssetProps) {
    const { uniqueId } = props;
    Object.assign(this, props, {
      uniqueId,
    });
  }

  @action
  update(props: Partial<AssetProps>) {
    const { uniqueId } = props;
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
      {
        uniqueId,
      }
    );
  }
}
