// AutoSizer measures its parent through offsetWidth/offsetHeight, which jsdom
// always reports as zero, so every virtualized list would render nothing under
// test. Only the measurement is faked here: List, WindowScroller and
// CellMeasurer stay real, so the tests still exercise the windowing logic.
jest.mock('react-virtualized', () => {
  const actual = jest.requireActual('react-virtualized');

  const AutoSizer = ({ children, disableHeight, disableWidth }) =>
    children({
      ...(disableWidth ? {} : { width: 1024 }),
      ...(disableHeight ? {} : { height: 768 }),
    });

  return { ...actual, AutoSizer };
});
