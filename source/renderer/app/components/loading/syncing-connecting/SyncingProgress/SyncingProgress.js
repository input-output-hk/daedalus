'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const classnames_1 = __importDefault(require('classnames'));
const react_1 = __importDefault(require('react'));
const bignumber_js_1 = __importDefault(require('bignumber.js'));
const react_intl_1 = require('react-intl');
const PopOver_1 = require('@react-polymorph/components/PopOver');
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const spinner_universal_inline_svg_1 = __importDefault(
  require('../../../../assets/images/spinner-universal.inline.svg')
);
const check_mark_universal_inline_svg_1 = __importDefault(
  require('../../../../assets/images/check-mark-universal.inline.svg')
);
const question_mark_universal_inline_svg_1 = __importDefault(
  require('../../../../assets/images/question-mark-universal.inline.svg')
);
const cardano_node_types_1 = require('../../../../../../common/types/cardano-node.types');
const SyncingProgress_scss_1 = __importDefault(
  require('./SyncingProgress.scss')
);
const utils_1 = require('./utils');
const logging_1 = require('../../../../utils/logging');
const blockSyncTypesOrdered = [
  cardano_node_types_1.BlockSyncType.validatingChunk,
  cardano_node_types_1.BlockSyncType.replayedBlock,
  cardano_node_types_1.BlockSyncType.pushingLedger,
];
const iconsColumnStyles = (0, classnames_1.default)(
  SyncingProgress_scss_1.default.column,
  SyncingProgress_scss_1.default.columnIcons
);
const messagesColumnStyles = (0, classnames_1.default)(
  SyncingProgress_scss_1.default.column,
  SyncingProgress_scss_1.default.columnMessages
);
const questionMarkIconStyles = (0, classnames_1.default)(
  SyncingProgress_scss_1.default.icon,
  SyncingProgress_scss_1.default.iconDescription
);
const makeLeftColumnIconStyles = (loading) =>
  (0, classnames_1.default)(
    SyncingProgress_scss_1.default.icon,
    SyncingProgress_scss_1.default.faded,
    {
      [SyncingProgress_scss_1.default.iconRotating]: loading,
    }
  );
const makeMainMessageStyles = (loaded) =>
  (0, classnames_1.default)({ [SyncingProgress_scss_1.default.faded]: loaded });
const makePercentageCellStyles = (loaded) =>
  (0, classnames_1.default)(
    SyncingProgress_scss_1.default.cell,
    SyncingProgress_scss_1.default.cellTextRight,
    {
      [SyncingProgress_scss_1.default.faded]: loaded,
    }
  );
const getSafePercentage = (value) => {
  try {
    return new bignumber_js_1.default(value).toFixed(2).toString();
  } catch (error) {
    logging_1.logger.error(
      'SyncingProgress::Percentage::Error parsing sync percentage',
      {
        error,
      }
    );
    return '-';
  }
};
function SyncingProgress(props, { intl }) {
  return react_1.default.createElement(
    'div',
    { className: SyncingProgress_scss_1.default.root },
    react_1.default.createElement(
      'div',
      { className: iconsColumnStyles },
      blockSyncTypesOrdered.map((type) =>
        react_1.default.createElement(
          'div',
          { key: type, className: SyncingProgress_scss_1.default.cell },
          react_1.default.createElement(react_svg_inline_1.default, {
            svg:
              props[type] < 100
                ? spinner_universal_inline_svg_1.default
                : check_mark_universal_inline_svg_1.default,
            className: makeLeftColumnIconStyles(props[type] < 100),
          })
        )
      )
    ),
    react_1.default.createElement(
      'div',
      { className: messagesColumnStyles },
      blockSyncTypesOrdered.map((type) =>
        react_1.default.createElement(
          'div',
          { key: type, className: SyncingProgress_scss_1.default.cell },
          react_1.default.createElement(
            'span',
            { className: makeMainMessageStyles(props[type] === 100) },
            intl.formatMessage(
              (0, utils_1.getProgressNameByBlockSyncType)(type)
            )
          ),
          react_1.default.createElement(
            'span',
            null,
            react_1.default.createElement(
              PopOver_1.PopOver,
              {
                content: intl.formatMessage(
                  (0, utils_1.getProgressDescriptionByBlockSyncType)(type)
                ),
              },
              react_1.default.createElement(react_svg_inline_1.default, {
                svg: question_mark_universal_inline_svg_1.default,
                className: questionMarkIconStyles,
              })
            )
          )
        )
      )
    ),
    react_1.default.createElement(
      'div',
      { className: SyncingProgress_scss_1.default.column },
      blockSyncTypesOrdered.map((type) =>
        react_1.default.createElement(
          'div',
          {
            key: type,
            className: makePercentageCellStyles(props[type] === 100),
          },
          getSafePercentage(props[type]),
          '%'
        )
      )
    )
  );
}
SyncingProgress.contextTypes = {
  intl: react_intl_1.intlShape.isRequired,
};
exports.default = SyncingProgress;
//# sourceMappingURL=SyncingProgress.js.map
