'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.TransactionMetadataView = void 0;
const react_1 = __importDefault(require('react'));
const json_bigint_1 = __importDefault(require('json-bigint'));
const TransactionMetadataView_scss_1 = __importDefault(
  require('./TransactionMetadataView.scss')
);
function flattenMetadata(data) {
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
    return data.list.map((v) => flattenMetadata(v));
  }
  // @ts-ignore ts-migrate(2339) FIXME: Property 'map' does not exist on type 'MetadataVal... Remove this comment to see the full error message
  if (data.map) {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'map' does not exist on type 'MetadataVal... Remove this comment to see the full error message
    return data.map.map((v) => {
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
function TransactionMetadataView(props) {
  return react_1.default.createElement(
    'div',
    { className: TransactionMetadataView_scss_1.default.root },
    Object.keys(props.data).map((key) =>
      react_1.default.createElement(
        'pre',
        { key: key },
        json_bigint_1.default.stringify(
          flattenMetadata(props.data[key]),
          null,
          2
        )
      )
    )
  );
}
exports.TransactionMetadataView = TransactionMetadataView;
//# sourceMappingURL=TransactionMetadataView.js.map
