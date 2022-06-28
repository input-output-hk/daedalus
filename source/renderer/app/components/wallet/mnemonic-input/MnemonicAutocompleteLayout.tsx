import React, {
  ChangeEventHandler,
  ClipboardEventHandler,
  MouseEventHandler,
  RefObject,
} from 'react';
import cx from 'classnames';
import { FormField } from 'react-polymorph/lib/components/FormField';
import { Options } from 'react-polymorph/lib/components/Options';
import { OptionsSkin } from 'react-polymorph/lib/skins/simple/OptionsSkin';
import styles from './MnemonicAutocompleteLayout.scss';

interface MnemonicInputSkinProps {
  onClick: MouseEventHandler<HTMLInputElement>;
  onChange: ChangeEventHandler<HTMLInputElement>;
  onSelect: (value: string) => void;
  onBlur: () => void;
  onPaste: ClipboardEventHandler<HTMLInputElement>;
  toggleMouseLocation: () => void;
  toggleOpen: () => void;

  value: string;
  ordinalNumber: number;
  disabled: boolean;
  error: boolean;
  isOpen: boolean;
  optionHeight?: number;
  options: string[];
  optionsMaxHeight: number;
  noResultsMessage: string;

  optionsRef: RefObject<any>;
  rootRef: RefObject<HTMLDivElement>;
  inputRef: RefObject<HTMLInputElement>;
  suggestionsRef: RefObject<HTMLDivElement>;
}

function MnemonicAutocompleteLayout({
  onClick,
  onChange,
  onSelect,
  onBlur,
  onPaste,
  toggleMouseLocation,
  toggleOpen,

  value,
  ordinalNumber,
  disabled,
  error,
  isOpen,
  optionHeight,
  options,
  optionsMaxHeight,
  noResultsMessage,

  optionsRef,
  rootRef,
  inputRef,
  suggestionsRef,
}: MnemonicInputSkinProps) {
  return (
    <div aria-hidden className={styles.root} ref={rootRef} role="presentation">
      <div className={styles.inputLabel}>{ordinalNumber}.</div>
      <FormField
        error={error}
        disabled={disabled}
        formFieldRef={inputRef}
        isErrorHidden={isOpen}
        render={(setFormFieldRef) => (
          <div ref={suggestionsRef} className={styles.inputWrapper}>
            <input
              className={cx(styles.input, error && styles.inputError)}
              ref={setFormFieldRef}
              value={value}
              onBlur={onBlur}
              onChange={onChange}
              onClick={onClick}
              onPaste={onPaste}
              disabled={disabled}
              spellCheck="false"
            />
          </div>
        )}
      />
      <Options
        isOpen={isOpen}
        noResults={!options.length}
        onChange={onSelect}
        options={options}
        persistSearchValue
        optionsRef={optionsRef}
        optionsMaxHeight={optionsMaxHeight}
        resetOnClose
        skin={OptionsSkin}
        targetRef={suggestionsRef}
        toggleMouseLocation={toggleMouseLocation}
        toggleOpen={toggleOpen}
        optionHeight={optionHeight}
        noResultsMessage={noResultsMessage}
      />
    </div>
  );
}

export { MnemonicAutocompleteLayout };
