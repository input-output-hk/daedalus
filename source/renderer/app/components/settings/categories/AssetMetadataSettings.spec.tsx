/**
 * The metadata source settings page.
 *
 * Rendered through `TestDecorator`, so every assertion is on text the real
 * `en-US.json` produces. A message with an id that is not in the catalogue
 * renders its own `!!!`-prefixed default and fails the case, which is what makes
 * these assertions worth writing against the copy rather than against test ids.
 */
import '@testing-library/jest-dom';

import React from 'react';
import noop from 'lodash/noop';
import {
  cleanup,
  fireEvent,
  render,
  screen,
  waitFor,
} from '@testing-library/react';

import { TestDecorator } from 'tests/_utils/TestDecorator';

import AssetMetadataSettings from './AssetMetadataSettings';
import { ASSET_METADATA_SERVERS_LIST } from '../../../config/assetsConfig';
import ApiError from '../../../domains/ApiError';

const KOIOS = ASSET_METADATA_SERVERS_LIST.koios.url;
const CUSTOM = 'https://koios.example.com/api/v1';

/**
 * The polymorph `Select` renders its selection and its option list at once, so
 * every option label is in the document twice. The selection is read from the
 * renderer element rather than by text, and the options by their position in
 * the list.
 */
const selectedLabel = (container: HTMLElement) =>
  container.querySelector('.selectionRenderer')?.textContent;

const renderPage = (props: Record<string, any> = {}) =>
  render(
    <TestDecorator>
      <AssetMetadataSettings
        sourceUrl={KOIOS}
        onSelectSourceUrl={noop}
        onResetSourceError={noop}
        isLoading={false}
        onOpenExternalLink={noop}
        {...props}
      />
    </TestDecorator>
  );

describe('AssetMetadataSettings', () => {
  afterEach(cleanup);

  it('selects the preset when the stored URL is the default', () => {
    const { container } = renderPage();
    expect(selectedLabel(container)).toBe('Koios (recommended)');
    expect(screen.queryByText('Index address')).not.toBeInTheDocument();
  });

  it('shows the address input when the stored URL is not a preset', () => {
    const { container } = renderPage({ sourceUrl: CUSTOM });
    expect(selectedLabel(container)).toBe('Custom index');
    expect(screen.getByText('Index address')).toBeInTheDocument();
    expect(screen.getByDisplayValue(CUSTOM)).toBeInTheDocument();
  });

  it('describes the selected source', () => {
    renderPage();
    expect(
      screen.getByText(/public, community-run index of the Cardano chain/)
    ).toBeInTheDocument();
    cleanup();
    renderPage({ sourceUrl: CUSTOM });
    expect(
      screen.getByText(/keeps this wallet out of any public index/)
    ).toBeInTheDocument();
  });

  it('says plainly what the selected index learns', () => {
    renderPage();
    expect(
      screen.getByText(/learns which of these tokens this wallet holds/)
    ).toBeInTheDocument();
  });

  it('offers all three sources', () => {
    renderPage();
    expect(screen.getByText('Custom index')).toBeInTheDocument();
    expect(
      screen.getByText('From my own chain data (not available yet)')
    ).toBeInTheDocument();
  });

  it('submits nothing when the unavailable source is chosen', () => {
    const onSelectSourceUrl = jest.fn();
    renderPage({ onSelectSourceUrl });
    fireEvent.click(
      screen.getByText('From my own chain data (not available yet)')
    );
    expect(onSelectSourceUrl).not.toHaveBeenCalled();
  });

  it('submits the preset URL when a preset is chosen', () => {
    const onSelectSourceUrl = jest.fn();
    const { container } = renderPage({ sourceUrl: CUSTOM, onSelectSourceUrl });
    expect(selectedLabel(container)).toBe('Custom index');
    fireEvent.click(screen.getByText('Koios (recommended)'));
    expect(onSelectSourceUrl).toHaveBeenCalledWith(KOIOS);
  });

  it('refuses an address that is not https and submits nothing', async () => {
    const onSelectSourceUrl = jest.fn();
    renderPage({ sourceUrl: CUSTOM, onSelectSourceUrl });
    const input = screen.getByDisplayValue(CUSTOM);
    // The error string only reaches the input while it is active, so a case
    // that types without focusing asserts against a component that is not
    // showing errors at all.
    fireEvent.focus(input);
    fireEvent.change(input, {
      target: { value: 'http://koios.example.com/api/v1' },
    });
    fireEvent.keyDown(input, { which: 13, keyCode: 13 });
    await waitFor(() =>
      expect(
        screen.getByText('The address should start with "https://"')
      ).toBeInTheDocument()
    );
    expect(onSelectSourceUrl).not.toHaveBeenCalled();
  });

  it('refuses an address carrying a query string and submits nothing', async () => {
    const onSelectSourceUrl = jest.fn();
    renderPage({ sourceUrl: CUSTOM, onSelectSourceUrl });
    const input = screen.getByDisplayValue(CUSTOM);
    // The error string only reaches the input while it is active, so a case
    // that types without focusing asserts against a component that is not
    // showing errors at all.
    fireEvent.focus(input);
    fireEvent.change(input, {
      target: { value: 'https://koios.example.com/api/v1?token=abc' },
    });
    fireEvent.keyDown(input, { which: 13, keyCode: 13 });
    await waitFor(() =>
      expect(screen.getByText('Invalid address')).toBeInTheDocument()
    );
    expect(onSelectSourceUrl).not.toHaveBeenCalled();
  });

  it('submits an address the validator accepts', async () => {
    const onSelectSourceUrl = jest.fn();
    renderPage({ sourceUrl: CUSTOM, onSelectSourceUrl });
    const input = screen.getByDisplayValue(CUSTOM);
    // The error string only reaches the input while it is active, so a case
    // that types without focusing asserts against a component that is not
    // showing errors at all.
    fireEvent.focus(input);
    fireEvent.change(input, {
      target: { value: 'https://other.example.com/api/v1' },
    });
    fireEvent.keyDown(input, { which: 13, keyCode: 13 });
    await waitFor(() =>
      expect(onSelectSourceUrl).toHaveBeenCalledWith(
        'https://other.example.com/api/v1'
      )
    );
  });

  // The two refusals from the probe are different facts about the instance and
  // the page has to say which one happened.
  it('renders an unreachable instance and a stale one differently', () => {
    renderPage({
      sourceUrl: CUSTOM,
      sourceUrlError: new ApiError({ code: 'invalid_asset_metadata_source' }),
    });
    expect(
      screen.getByText('This URL did not answer as a metadata source')
    ).toBeInTheDocument();
    cleanup();
    renderPage({
      sourceUrl: CUSTOM,
      sourceUrlError: new ApiError({ code: 'stale_asset_metadata_source' }),
    });
    expect(
      screen.getByText(
        'This metadata source is too far behind your node to be used'
      )
    ).toBeInTheDocument();
  });

  it('clears the error when it is unmounted', () => {
    const onResetSourceError = jest.fn();
    const { unmount } = renderPage({ onResetSourceError });
    unmount();
    expect(onResetSourceError).toHaveBeenCalled();
  });
});
