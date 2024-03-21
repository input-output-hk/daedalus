'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importDefault(require('react'));
require('./TransactionMetadataView.scss');
/**
 * NOTE: These components are currently not used because we simply
 * JSON.stringify the metadata for transactions. This can be
 * used as the basis for a more sophisticated implementation
 * later on:
 */
function IntegerView({ value }) {
  return react_1.default.createElement('p', null, value.int);
}
function StringView({ value }) {
  return react_1.default.createElement('p', null, value.string);
}
function BytesView({ value }) {
  return react_1.default.createElement('p', null, value.bytes);
}
function ListView({ value }) {
  return react_1.default.createElement(
    'ol',
    null,
    value.list.map((
      v,
      index // eslint-disable-next-line react/no-array-index-key
    ) =>
      react_1.default.createElement(
        'li',
        { key: index },
        react_1.default.createElement(MetadataValueView, { value: v })
      )
    )
  );
}
function MapView({ value }) {
  return react_1.default.createElement(
    'ol',
    null,
    value.map.map((
      v,
      index // eslint-disable-next-line react/no-array-index-key
    ) =>
      react_1.default.createElement(
        'li',
        { key: index },
        react_1.default.createElement(MetadataValueView, { value: v.k }),
        ': ',
        react_1.default.createElement(MetadataValueView, { value: v.v })
      )
    )
  );
}
function MetadataValueView(props) {
  const { value } = props;
  // @ts-ignore ts-migrate(2339) FIXME: Property 'int' does not exist on type 'MetadataVal... Remove this comment to see the full error message
  if (value.int) {
    // @ts-ignore ts-migrate(2322) FIXME: Type 'MetadataValue' is not assignable to type 'Me... Remove this comment to see the full error message
    return react_1.default.createElement(IntegerView, { value: value });
  }
  // @ts-ignore ts-migrate(2339) FIXME: Property 'string' does not exist on type 'Metadata... Remove this comment to see the full error message
  if (value.string) {
    // @ts-ignore ts-migrate(2322) FIXME: Type 'MetadataValue' is not assignable to type 'Me... Remove this comment to see the full error message
    return react_1.default.createElement(StringView, { value: value });
  }
  // @ts-ignore ts-migrate(2339) FIXME: Property 'bytes' does not exist on type 'MetadataV... Remove this comment to see the full error message
  if (value.bytes) {
    // @ts-ignore ts-migrate(2322) FIXME: Type 'MetadataValue' is not assignable to type 'Me... Remove this comment to see the full error message
    return react_1.default.createElement(BytesView, { value: value });
  }
  // @ts-ignore ts-migrate(2339) FIXME: Property 'list' does not exist on type 'MetadataVa... Remove this comment to see the full error message
  if (value.list) {
    // @ts-ignore ts-migrate(2322) FIXME: Type 'MetadataValue' is not assignable to type 'Me... Remove this comment to see the full error message
    return react_1.default.createElement(ListView, { value: value });
  }
  // @ts-ignore ts-migrate(2339) FIXME: Property 'map' does not exist on type 'MetadataVal... Remove this comment to see the full error message
  if (value.map) {
    // @ts-ignore ts-migrate(2322) FIXME: Type 'MetadataValue' is not assignable to type 'Me... Remove this comment to see the full error message
    return react_1.default.createElement(MapView, { value: value });
  }
  return null;
}
//# sourceMappingURL=MetadataValueView.js.map
