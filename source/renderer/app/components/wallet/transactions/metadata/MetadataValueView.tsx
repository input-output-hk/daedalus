import React from 'react';
import type {
  MetadataBytes,
  MetadataInteger,
  MetadataList,
  MetadataMap,
  MetadataString,
  MetadataValue,
} from '../../../../types/TransactionMetadata';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './TransactionMetadataView.scss... Remove this comment to see the full error message
import styles from './TransactionMetadataView.scss';

/**
 * NOTE: These components are currently not used because we simply
 * JSON.stringify the metadata for transactions. This can be
 * used as the basis for a more sophisticated implementation
 * later on:
 */
function IntegerView({ value }: { value: MetadataInteger }) {
  return <p>{value.int}</p>;
}

function StringView({ value }: { value: MetadataString }) {
  return <p>{value.string}</p>;
}

function BytesView({ value }: { value: MetadataBytes }) {
  return <p>{value.bytes}</p>;
}

function ListView({ value }: { value: MetadataList }) {
  return (
    <ol className={styles.list}>
      {value.list.map((
        v,
        index // eslint-disable-next-line react/no-array-index-key
      ) => (
        <li key={index}>
          <MetadataValueView value={v} />
        </li>
      ))}
    </ol>
  );
}

function MapView({ value }: { value: MetadataMap }) {
  return (
    <ol className={styles.map}>
      {value.map.map((
        v,
        index // eslint-disable-next-line react/no-array-index-key
      ) => (
        <li key={index}>
          <MetadataValueView value={v.k} />: <MetadataValueView value={v.v} />
        </li>
      ))}
    </ol>
  );
}

function MetadataValueView(props: { value: MetadataValue }) {
  const { value } = props;

  // @ts-ignore ts-migrate(2339) FIXME: Property 'int' does not exist on type 'MetadataVal... Remove this comment to see the full error message
  if (value.int) {
    // @ts-ignore ts-migrate(2322) FIXME: Type 'MetadataValue' is not assignable to type 'Me... Remove this comment to see the full error message
    return <IntegerView value={value} />;
  }

  // @ts-ignore ts-migrate(2339) FIXME: Property 'string' does not exist on type 'Metadata... Remove this comment to see the full error message
  if (value.string) {
    // @ts-ignore ts-migrate(2322) FIXME: Type 'MetadataValue' is not assignable to type 'Me... Remove this comment to see the full error message
    return <StringView value={value} />;
  }

  // @ts-ignore ts-migrate(2339) FIXME: Property 'bytes' does not exist on type 'MetadataV... Remove this comment to see the full error message
  if (value.bytes) {
    // @ts-ignore ts-migrate(2322) FIXME: Type 'MetadataValue' is not assignable to type 'Me... Remove this comment to see the full error message
    return <BytesView value={value} />;
  }

  // @ts-ignore ts-migrate(2339) FIXME: Property 'list' does not exist on type 'MetadataVa... Remove this comment to see the full error message
  if (value.list) {
    // @ts-ignore ts-migrate(2322) FIXME: Type 'MetadataValue' is not assignable to type 'Me... Remove this comment to see the full error message
    return <ListView value={value} />;
  }

  // @ts-ignore ts-migrate(2339) FIXME: Property 'map' does not exist on type 'MetadataVal... Remove this comment to see the full error message
  if (value.map) {
    // @ts-ignore ts-migrate(2322) FIXME: Type 'MetadataValue' is not assignable to type 'Me... Remove this comment to see the full error message
    return <MapView value={value} />;
  }

  return null;
}
