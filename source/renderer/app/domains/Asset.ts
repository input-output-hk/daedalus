import { pick } from 'lodash';
import { observable, action, computed } from 'mobx';
import type { Asset as AssetProps, AssetMetadata } from '../api/assets/types';
import type { AssetMetadataSource } from '../../../common/types/asset-metadata.types';
import { hexToString } from '../utils/strings';

export default class Asset {
  @observable
  policyId = '';
  @observable
  assetName = '';
  @observable
  uniqueId = '';
  @observable
  fingerprint = '';
  @observable
  metadata: AssetMetadata | null | undefined;
  @observable
  decimals: number | null | undefined;
  @observable
  recommendedDecimals: number | null | undefined;
  @observable
  recommendedDecimalsVerified: boolean | null | undefined;
  @observable
  hasImage: boolean | null | undefined;
  @observable
  source: AssetMetadataSource | null | undefined;

  @computed
  get assetNameASCII() {
    return hexToString(this.assetName || '');
  }

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
        'recommendedDecimalsVerified',
        'hasImage',
        'source',
      ]),
      {
        uniqueId,
      }
    );
  }
}
