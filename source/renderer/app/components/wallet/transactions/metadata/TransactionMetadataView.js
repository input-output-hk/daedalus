// @flow
import React from 'react';
import JSONBigInt from 'json-bigint';
import type {
  MetadataMapValue,
  MetadataValue,
  TransactionMetadata,
} from '../../../../types/TransactionMetadata';
import styles from './TransactionMetadataView.scss';

function flattenMetadata(data: MetadataValue) {
  if (data.int) {
    return data.int;
  }
  if (data.string != null) {
    return data.string;
  }
  if (data.bytes) {
    return `0x${data.bytes}`;
  }
  if (data.list) {
    return data.list.map((v: MetadataValue) => flattenMetadata(v));
  }
  if (data.map) {
    return data.map.map((v: MetadataMapValue) => {
      if (v.k.list || v.k.map) {
        return {
          key: flattenMetadata(v.k),
          value: flattenMetadata(v.v),
        };
      }
      if (v.k.int) {
        return { [v.k.int]: flattenMetadata(v.v) };
      }
      if (v.k.string) {
        return { [v.k.string]: flattenMetadata(v.v) };
      }
      if (v.k.bytes) {
        return { [v.k.bytes]: flattenMetadata(v.v) };
      }
      return null;
    });
  }
  return null;
}

export function TransactionMetadataView(props: { data: TransactionMetadata }) {
  return (
    <div className={styles.root}>
      {Object.keys(props.data).map((key: string) => (
        <pre key={key}>
          {JSONBigInt.stringify(flattenMetadata(props.data[key]), null, 2)}
        </pre>
      ))}
    </div>
  );
}
