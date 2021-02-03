// @flow
import React from 'react';
import type {
  MetadataBytes,
  MetadataInteger,
  MetadataList,
  MetadataMap,
  MetadataString,
  MetadataValue,
  TransactionMetadata,
} from '../../../../types/TransactionMetadata';
import styles from './TransactionMetadataView.scss';

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
      {value.list.map((v) => (
        <li>
          <MetadataValueView value={v} />
        </li>
      ))}
    </ol>
  );
}

function MapView({ value }: { value: MetadataMap }) {
  return (
    <ol className={styles.map}>
      {value.map.map((v) => (
        <li>
          <MetadataValueView value={v.k} />: <MetadataValueView value={v.v} />
        </li>
      ))}
    </ol>
  );
}

function MetadataValueView(props: { value: MetadataValue }) {
  const { value } = props;
  if (value.int) {
    return <IntegerView value={value} />;
  }
  if (value.string) {
    return <StringView value={value} />;
  }
  if (value.bytes) {
    return <BytesView value={value} />;
  }
  if (value.list) {
    return <ListView value={value} />;
  }
  if (value.map) {
    return <MapView value={value} />;
  }
  return null;
}

export function TransactionMetadataView(props: { data: TransactionMetadata }) {
  return (
    <div className={styles.root}>
      {Object.keys(props.data).map((key: string) => (
        <MetadataValueView value={props.data[key]} />
      ))}
    </div>
  );
}
