export interface ApiDRepMetaReference {
  '@type'?: string;
  label: string | null;
  uri: string;
}

export interface ApiDRepMetadata {
  name: string;
  objectives: string | null;
  motivations: string | null;
  qualifications: string | null;
  payment_address?: string;
  do_not_list: boolean;
  references: ApiDRepMetaReference[];
}

export interface ApiDRepInfo {
  id: string;
  credential: {
    type: 'key_hash' | 'script_hash';
    hash: string;
  };
  status: 'active' | 'inactive';
  expiry_epoch: number;
  voting_power: {
    quantity: string;
    unit: 'lovelace';
  };
  deposit: {
    quantity: number;
    unit: 'lovelace';
  };
  anchor: {
    url: string;
    data_hash: string;
  } | null;
  name: string | null;
  do_not_list?: boolean;
  metadata: ApiDRepMetadata | null;
}
