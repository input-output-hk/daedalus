import React from 'react';
import JSONBigInt from 'json-bigint';
import type {
  MetadataMapValue,
  MetadataValue,
  TransactionMetadata,
} from '../../../../types/TransactionMetadata';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './TransactionMetadataView.scss... Remove this comment to see the full error message
import styles from './TransactionMetadataView.scss';

function flattenMetadata(data: MetadataValue) {
  // @ts-ignore ts-migrate(2339) FIXME: Property 'int' does not exist on type 'MetadataVal... Remove this comment to see the full error message
  if (data.int) {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'int' does not exist on type 'MetadataVal... Remove this comment to see the full error message
    return data.int;
  }

  // @ts-ignore ts-migrate(2339) FIXME: Property 'string' does not exist on type 'Metadata... Remove this comment to see the full error message
  if (data.string != null) {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'string' does not exist on type 'Metadata... Remove this comment to see the full error message
    return data.string;
  }

  // @ts-ignore ts-migrate(2339) FIXME: Property 'bytes' does not exist on type 'MetadataV... Remove this comment to see the full error message
  if (data.bytes) {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'bytes' does not exist on type 'MetadataV... Remove this comment to see the full error message
    return `0x${data.bytes}`;
  }

  // @ts-ignore ts-migrate(2339) FIXME: Property 'list' does not exist on type 'MetadataVa... Remove this comment to see the full error message
  if (data.list) {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'list' does not exist on type 'MetadataVa... Remove this comment to see the full error message
    return data.list.map((v: MetadataValue) => flattenMetadata(v));
  }

  // @ts-ignore ts-migrate(2339) FIXME: Property 'map' does not exist on type 'MetadataVal... Remove this comment to see the full error message
  if (data.map) {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'map' does not exist on type 'MetadataVal... Remove this comment to see the full error message
    return data.map.map((v: MetadataMapValue) => {
      // @ts-ignore ts-migrate(2339) FIXME: Property 'list' does not exist on type 'MetadataVa... Remove this comment to see the full error message
      if (v.k.list || v.k.map) {
        return {
          key: flattenMetadata(v.k),
          value: flattenMetadata(v.v),
        };
      }

      // @ts-ignore ts-migrate(2339) FIXME: Property 'int' does not exist on type 'MetadataVal... Remove this comment to see the full error message
      if (v.k.int) {
        return {
          // @ts-ignore ts-migrate(2339) FIXME: Property 'int' does not exist on type 'MetadataVal... Remove this comment to see the full error message
          [v.k.int]: flattenMetadata(v.v),
        };
      }

      // @ts-ignore ts-migrate(2339) FIXME: Property 'string' does not exist on type 'Metadata... Remove this comment to see the full error message
      if (v.k.string) {
        return {
          // @ts-ignore ts-migrate(2339) FIXME: Property 'string' does not exist on type 'Metadata... Remove this comment to see the full error message
          [v.k.string]: flattenMetadata(v.v),
        };
      }

      // @ts-ignore ts-migrate(2339) FIXME: Property 'bytes' does not exist on type 'MetadataV... Remove this comment to see the full error message
      if (v.k.bytes) {
        return {
          // @ts-ignore ts-migrate(2339) FIXME: Property 'bytes' does not exist on type 'MetadataV... Remove this comment to see the full error message
          [v.k.bytes]: flattenMetadata(v.v),
        };
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
