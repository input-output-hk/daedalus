'use strict';
var __createBinding =
  (this && this.__createBinding) ||
  (Object.create
    ? function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        var desc = Object.getOwnPropertyDescriptor(m, k);
        if (
          !desc ||
          ('get' in desc ? !m.__esModule : desc.writable || desc.configurable)
        ) {
          desc = {
            enumerable: true,
            get: function () {
              return m[k];
            },
          };
        }
        Object.defineProperty(o, k2, desc);
      }
    : function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        o[k2] = m[k];
      });
var __setModuleDefault =
  (this && this.__setModuleDefault) ||
  (Object.create
    ? function (o, v) {
        Object.defineProperty(o, 'default', { enumerable: true, value: v });
      }
    : function (o, v) {
        o['default'] = v;
      });
var __importStar =
  (this && this.__importStar) ||
  function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null)
      for (var k in mod)
        if (k !== 'default' && Object.prototype.hasOwnProperty.call(mod, k))
          __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
  };
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.StakePoolsTable = exports.defaultTableOrdering = void 0;
const react_1 = __importStar(require('react'));
const mobx_react_1 = require('mobx-react');
const classnames_1 = __importDefault(require('classnames'));
const react_intl_1 = require('react-intl');
const react_table_1 = require('react-table');
const List_1 = __importDefault(require('react-virtualized/dist/commonjs/List'));
const WindowScroller_1 = __importDefault(
  require('react-virtualized/dist/commonjs/WindowScroller')
);
const AutoSizer_1 = __importDefault(
  require('react-virtualized/dist/commonjs/AutoSizer')
);
const StakingWithNavigation_1 = require('../layouts/StakingWithNavigation');
const StakePoolsTable_scss_1 = __importDefault(
  require('./StakePoolsTable.scss')
);
const LoadingSpinner_1 = __importDefault(
  require('../../widgets/LoadingSpinner')
);
const BorderedBox_1 = __importDefault(require('../../widgets/BorderedBox'));
const hooks_1 = require('./hooks');
const StakePoolsTableHeader_1 = require('./StakePoolsTableHeader');
exports.defaultTableOrdering = {
  ranking: hooks_1.StakePoolsOrder.Asc,
  ticker: hooks_1.StakePoolsOrder.Asc,
  saturation: hooks_1.StakePoolsOrder.Asc,
  cost: hooks_1.StakePoolsOrder.Asc,
  profitMargin: hooks_1.StakePoolsOrder.Asc,
  producedBlocks: hooks_1.StakePoolsOrder.Desc,
  nonMyopicMemberRewards: hooks_1.StakePoolsOrder.Desc,
  pledge: hooks_1.StakePoolsOrder.Asc,
  retiring: hooks_1.StakePoolsOrder.Asc,
};
// Maximum number of stake pools for which we do not need to use the preloading
const PRELOADER_THRESHOLD = 100;
const initialState = {
  isPreloading: true,
  stakePoolsOrder: hooks_1.StakePoolsOrder.Asc,
  stakePoolsSortBy: 'ranking',
};
function StakePoolsTableComponent({
  stakePoolsList = [],
  listName,
  onTableHeaderMouseEnter,
  onTableHeaderMouseLeave,
  showWithSelectButton = false,
  intl,
  numberOfRankedStakePools,
  currentTheme,
  onOpenExternalLink,
  containerClassName,
  onSelect,
}) {
  const [state, setState] = (0, react_1.useState)(initialState);
  (0, react_1.useEffect)(() => {
    setState((s) => ({ ...s, isPreloading: false }));
  }, []);
  const handleSort = (0, react_1.useCallback)(
    (newSortBy) => {
      const { stakePoolsOrder, stakePoolsSortBy } = state;
      let newOrder = exports.defaultTableOrdering[newSortBy];
      if (newSortBy === stakePoolsSortBy) {
        newOrder =
          stakePoolsOrder === hooks_1.StakePoolsOrder.Asc
            ? hooks_1.StakePoolsOrder.Desc
            : hooks_1.StakePoolsOrder.Asc;
      }
      setState((s) => ({
        ...s,
        stakePoolsOrder: newOrder,
        stakePoolsSortBy: newSortBy,
      }));
    },
    [state]
  );
  const { isPreloading, stakePoolsSortBy, stakePoolsOrder } = state;
  const sortedStakePoolList = (0, hooks_1.useSortedStakePoolList)({
    order: stakePoolsOrder,
    sortBy: stakePoolsSortBy,
    stakePoolList: stakePoolsList,
  });
  const columns = (0, hooks_1.useCreateColumns)({
    containerClassName,
    currentTheme,
    intl,
    numberOfRankedStakePools,
    onOpenExternalLink,
    onSelect,
    showWithSelectButton,
  });
  const {
    getTableProps,
    getTableBodyProps,
    headerGroups,
    rows,
    prepareRow,
  } = (0, react_table_1.useTable)(
    {
      columns,
      data: sortedStakePoolList,
    },
    react_table_1.useFlexLayout
  );
  const RenderRow = (0, react_1.useCallback)(
    ({ index, style }) => {
      const row = rows[index];
      prepareRow(row);
      return react_1.default.createElement(
        'div',
        {
          ...row.getRowProps({
            style,
          }),
          className: StakePoolsTable_scss_1.default.tr,
        },
        row.cells.map((cell) => {
          return (
            /* eslint-disable-next-line react/jsx-key */
            react_1.default.createElement(
              'div',
              {
                ...cell.getCellProps({ style: { width: undefined } }),
                className: StakePoolsTable_scss_1.default.td,
              },
              cell.render('Cell')
            )
          );
        })
      );
    },
    [prepareRow, rows]
  );
  const componentClasses = (0, classnames_1.default)([
    StakePoolsTable_scss_1.default.component,
    listName,
  ]);
  if (stakePoolsList.length > PRELOADER_THRESHOLD && isPreloading)
    return react_1.default.createElement(
      'div',
      { className: StakePoolsTable_scss_1.default.preloadingBlockWrapper },
      react_1.default.createElement(LoadingSpinner_1.default, { big: true })
    );
  return react_1.default.createElement(
    StakingWithNavigation_1.StakingPageScrollContext.Consumer,
    null,
    (stakePoolsScrollContext) =>
      react_1.default.createElement(
        WindowScroller_1.default,
        { scrollElement: stakePoolsScrollContext.scrollElementRef.current },
        ({ height, isScrolling, registerChild, onChildScroll, scrollTop }) =>
          react_1.default.createElement(
            'div',
            null,
            react_1.default.createElement(
              'div',
              { className: componentClasses },
              sortedStakePoolList.length > 0 &&
                react_1.default.createElement(
                  BorderedBox_1.default,
                  null,
                  react_1.default.createElement(
                    'div',
                    {
                      ...getTableProps(),
                      className: StakePoolsTable_scss_1.default.table,
                    },
                    react_1.default.createElement(
                      StakePoolsTableHeader_1.StakePoolsTableHeader,
                      {
                        stakePoolsSortBy: stakePoolsSortBy,
                        stakePoolsOrder: stakePoolsOrder,
                        headerGroups: headerGroups,
                        onHandleSort: handleSort,
                        onTableHeaderMouseEnter: onTableHeaderMouseEnter,
                        onTableHeaderMouseLeave: onTableHeaderMouseLeave,
                      }
                    ),
                    react_1.default.createElement(
                      AutoSizer_1.default,
                      { disableHeight: true },
                      ({ width }) =>
                        react_1.default.createElement(
                          'div',
                          {
                            className: StakePoolsTable_scss_1.default.tbody,
                            ...getTableBodyProps(),
                            ref: registerChild,
                          },
                          react_1.default.createElement(List_1.default, {
                            autoHeight: true,
                            height: height || 0,
                            isScrolling: isScrolling,
                            onScroll: onChildScroll,
                            overscanRowCount: 10,
                            rowCount: rows.length,
                            rowHeight: 36,
                            rowRenderer: RenderRow,
                            scrollTop: scrollTop,
                            width: width,
                          })
                        )
                    )
                  )
                )
            )
          )
      )
  );
}
exports.StakePoolsTable = (0, react_intl_1.injectIntl)(
  (0, mobx_react_1.observer)(StakePoolsTableComponent)
);
//# sourceMappingURL=StakePoolsTable.js.map
