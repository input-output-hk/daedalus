import React, { RefObject, useCallback } from 'react';
import cx from 'classnames';
import { FormField } from 'react-polymorph/lib/components/FormField';
import { Options } from 'react-polymorph/lib/components/Options';
import { OptionsSkin } from 'react-polymorph/lib/skins/simple/OptionsSkin';
import * as styles from './MnemonicAutocompleteLayout.scss';

interface MnemonicInputSkinProps {
  onClick: () => void;
  onChange: (value: string) => void;
  onSelect: (value: string) => void;
  onBlur: () => void;
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

const MnemonicAutocompleteLayout = ({
  onClick,
  onChange,
  onSelect,
  onBlur,
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
}: MnemonicInputSkinProps) => {
  const handleInputChange = useCallback(
    (event: React.FormEvent<HTMLInputElement>) => {
      const { value: inputValue } = event.currentTarget;

      onChange(inputValue);
    },
    []
  );

  return (
    <div
      aria-hidden
      className={styles.root}
      onClick={onClick}
      ref={rootRef}
      role="presentation"
    >
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
              onChange={handleInputChange}
              onBlur={onBlur}
              disabled={disabled}
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
};

export { MnemonicAutocompleteLayout };
