'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.StakePoolsTableHeader = exports.Component = void 0;
const react_1 = __importDefault(require('react'));
const mobx_react_1 = require('mobx-react');
const classnames_1 = __importDefault(require('classnames'));
const StakePoolsTableHeaderCell_1 = require('./StakePoolsTableHeaderCell');
const hooks_1 = require('./hooks');
const StakePoolsTable_scss_1 = __importDefault(
  require('./StakePoolsTable.scss')
);
function Component({
  stakePoolsSortBy,
  stakePoolsOrder,
  headerGroups,
  onHandleSort,
  onTableHeaderMouseEnter,
  onTableHeaderMouseLeave,
}) {
  const { setTargetRef, isInViewport } = (0, hooks_1.useInViewPort)();
  return react_1.default.createElement(
    react_1.default.Fragment,
    null,
    react_1.default.createElement('div', { ref: setTargetRef }),
    react_1.default.createElement(
      'div',
      {
        className: (0, classnames_1.default)(
          StakePoolsTable_scss_1.default.thead,
          !isInViewport && StakePoolsTable_scss_1.default.stickyHeader
        ),
        onMouseEnter: onTableHeaderMouseEnter,
        onMouseLeave: onTableHeaderMouseLeave,
      },
      headerGroups.map((headerGroup) =>
        /* eslint-disable-next-line react/jsx-key */
        react_1.default.createElement(
          'div',
          {
            ...headerGroup.getHeaderGroupProps(),
            className: StakePoolsTable_scss_1.default.tr,
          },
          headerGroup.headers.map((column) =>
            react_1.default.createElement(
              StakePoolsTableHeaderCell_1.StakePoolsTableHeaderCell,
              {
                ...column.getHeaderProps({
                  style: { width: undefined },
                }),
                stakePoolsSortBy: stakePoolsSortBy,
                stakePoolsOrder: stakePoolsOrder,
                onHandleSort: onHandleSort,
                name: column.id,
                key: column.id,
              },
              column.render('Header')
            )
          )
        )
      )
    )
  );
}
exports.Component = Component;
exports.StakePoolsTableHeader = (0, mobx_react_1.observer)(Component);
//# sourceMappingURL=StakePoolsTableHeader.js.map
