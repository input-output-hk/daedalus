// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { AutoSizer, List } from 'react-virtualized';
import type { Addresses, Address } from '../../../api/addresses/types';
import styles from './VirtualAddressesList.scss';

type Props = {
  rows: Addresses,
  renderRow: Function,
};

type RowHeight = number;
type RowLine = number;
type Breakpoint = number;
type IndividualLine = number;


/**
 *
 * There are 2 breakpoints (2 and 4) where the
 * rows height need to be individually calculated,
 * as the addresses may have different line numbers.
 * Otherwise, the heights can be safely calculated
 * at once, which has much less cost.
 *
 * Considering the width of the
 * `.ReactVirtualized__Grid__innerScrollContainer` element:
 *
 * | Breakpoint| Width             | Lines  | Calculation |
 * |-----------|-------------------|--------|-------------|
 * | 1         |            < 600  | 3      | general     |
 * | 2         | >= 600  && < 700  | 2 or 3 | individual  |
 * | 3         | >= 700  && < 1100 | 2      | general     |
 * | 4         | >= 1100 && > 1200 | 1 or 2 | individual  |
 * | 5         | >= 1200           | 1      | general     |
 *
 */

const BREAKPOINT_2 = 600;
const BREAKPOINT_3 = 700;
const BREAKPOINT_4 = 1100;
const BREAKPOINT_5 = 1200;

const BREAKPOINT_WITH_GENERAL_CALCULATION = 'general';
const BREAKPOINT_WITH_INDIVIDUAL_CALCULATION = 'individual';

const ADDRESS_LINE_HEIGHT = 22;
const ADDRESS_LINE_PADDING = 21;

const INDIVIDUAL_CALCULATION_WAIT_TIMEOUT = 50;

const ADDRESS_HEIGHT_BREAKPOINT_TWO_LINES = 25;
const ADDRESS_HEIGHT_BREAKPOINT_THREE_LINES = 50;

@observer
export class VirtualAddressesList extends Component<Props> {

  list: List;
  rowHeights: RowHeight[] = [];
  rowLines: RowLine[] = [];
  individualCalculationTimeout: ?TimeoutID = null;
  currentBreakpoint: Breakpoint = 0;
  genericNumberOfLines: number = 0;
  individualNumberOfLines: IndividualLine[] = [];
  width: number = 0;

  /**
   * Calculate the height of the addressess generically,
   * based on the container width
   */
  calculateGeneralRowHeights = () => {
    const { rows } = this.props;
    const { genericNumberOfLines } = this;
    const height = this.getHeightFromNumberOfLines(genericNumberOfLines);
    rows.forEach((row: Address, index: number) => {
      this.rowHeights[index] = height;
      this.list.recomputeRowHeights(index);
    });
  }

  /**
   * Calculate the height of the addressess individually,
   * based on their DOM element height
   */
  calculateIndividualRowHeights = () => {
    // If the page just loaded, trigger another update
    // for when the list is rendered
    if (this.genericNumberOfLines === 0) {
      this.updateLines();
      setTimeout(this.calculateIndividualRowHeights, 100);
    }
    this.props.rows.forEach(this.calculateRowHeight);
  }

  /**
   * Calculate the height of the given row
   * based on its DOM element height
   * @param row
   * @param index
   */
  calculateRowHeight = (row: Address, index: number) => {

    const { id } = row;
    const rowElement = this.getAddressRowElementById(id);
    let rowHeight = ADDRESS_LINE_HEIGHT;
    if (rowElement instanceof HTMLElement) rowHeight = rowElement.offsetHeight;
    const rowLines = this.getRowLinesFromHeight(rowHeight);

    // It will only update the DOM element if it actually changed its height
    if (rowLines === this.individualNumberOfLines[index] && this.genericNumberOfLines !== 0) return;

    const height = this.getHeightFromNumberOfLines(rowLines);
    this.rowHeights[index] = height;
    this.individualNumberOfLines[index] = rowLines;
    this.list.recomputeRowHeights(index);
  }

  /**
   * Gets the breakpoint based on the container width
   * @param width
   * @returns {number}
   */
  getBreakpointFromWidth = (width: number) => {
    if (width >= BREAKPOINT_5) return 5;
    if (width >= BREAKPOINT_4) return 4;
    if (width >= BREAKPOINT_3) return 3;
    if (width >= BREAKPOINT_2) return 2;
    return 1;
  }

  /**
   * Updates the current number of lines for the addresses
   */
  updateLines = () => {
    const { currentBreakpoint } = this;
    this.genericNumberOfLines = this.getLinesFromBreakpoint(currentBreakpoint);
    this.props.rows.forEach((x, i) => this.individualNumberOfLines[i] = this.genericNumberOfLines);
  }

  /**
   * Gets the breakpoint calculation type
   * @param breakpoint
   * @returns {string}
   */
  getBreakpointCalculationType = (breakpoint: Breakpoint) => {
    if (breakpoint === 2 || breakpoint === 4) return BREAKPOINT_WITH_INDIVIDUAL_CALCULATION;
    return BREAKPOINT_WITH_GENERAL_CALCULATION;
  }

  /**
   * Gets the number of lines of the given breakpoint
   * @param breakpoint
   * @returns {number}
   */
  getLinesFromBreakpoint = (breakpoint: Breakpoint) => {
    if (breakpoint === 1) return 3;
    if (breakpoint === 3) return 2;
    if (breakpoint === 5) return 1;
    return 0;
  }

  /**
   * Gets the number of lines based on the row's height
   * @param height
   * @returns {number}
   */
  getRowLinesFromHeight = (height: number) => {
    if (height > ADDRESS_HEIGHT_BREAKPOINT_THREE_LINES) return 3;
    if (height > ADDRESS_HEIGHT_BREAKPOINT_TWO_LINES) return 2;
    return 1;
  }

  /**
   * Calculates the row's height based on the number of lines
   * @param lines
   * @returns {number}
   */
  getHeightFromNumberOfLines = (lines: number) => (
    (ADDRESS_LINE_HEIGHT * lines) + ADDRESS_LINE_PADDING
  );

  /**
   * Decides if the addresses heights need to be updated
   * and which type of calculation, generic or individual
   */
  onResize = ({ width }: { width: number }) => {
    this.width = width;
    const newBreakpoint = this.getBreakpointFromWidth(width);
    const breakpointType = this.getBreakpointCalculationType(newBreakpoint);
    let didChangBreakpoint = false;

    if (newBreakpoint !== this.currentBreakpoint) {
      this.currentBreakpoint = newBreakpoint;
      didChangBreakpoint = true;
    }

    /**
     * The breakpoint requires individual calculation,
     * since the addresses height may vary
     *
     * The timeout avoids unecessary DOM manupulation
     * while the window is still being resized
     */
    if (breakpointType === BREAKPOINT_WITH_INDIVIDUAL_CALCULATION) {
      this.cancelIndividualCalculationTimeout();
      this.individualCalculationTimeout = setTimeout(
        this.calculateIndividualRowHeights,
        INDIVIDUAL_CALCULATION_WAIT_TIMEOUT
      );

    /**
     * The new breakpoint allows for generic calculation
     * based on the container width
     */
    } else if (didChangBreakpoint) {
      this.updateLines();
      this.calculateGeneralRowHeights();
    }

  }

  cancelIndividualCalculationTimeout = () => {
    if (this.individualCalculationTimeout) {
      clearTimeout(this.individualCalculationTimeout);
    }
  }

  getAddressRowElementById = (id: string) => (
    document.getElementById(`address-${id}`)
  );

  rowRenderer = ({
    index, // Index of row
    key, // Unique key within array of rendered rows
    style // Style object to be applied to row (to position it);
  }: { key: string, index: number, style: string }) => {
    const { rows, renderRow } = this.props;
    const address = rows[index];
    return (
      <div
        key={key}
        style={style}
        className={styles.address}
      >
        {renderRow(address, index)}
      </div>
    );
  }

  getRowHeights = ({ index }: { index: number }) => this.rowHeights[index] ||
    this.getHeightFromNumberOfLines(this.genericNumberOfLines);

  render() {
    const { rows } = this.props;
    if (!rows.length) return null;
    return (
      <div className={styles.component}>
        <AutoSizer onResize={this.onResize}>
          {({ width, height }) => (
            <List
              className={styles.list}
              ref={(list) => this.list = list}
              width={width}
              height={height}
              rowCount={rows.length}
              rowHeight={this.getRowHeights}
              rowRenderer={this.rowRenderer}
            />
          )}
        </AutoSizer>
      </div>
    );
  }
}
