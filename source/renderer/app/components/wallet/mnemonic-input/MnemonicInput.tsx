import React, {
  ClipboardEventHandler,
  useCallback,
  useEffect,
  useMemo,
  useRef,
  VFC,
} from 'react';
import { defineMessages, injectIntl } from 'react-intl';
import { chunk, constant, times } from 'lodash';
import cx from 'classnames';
import { INCOMPLETE_MNEMONIC_MARKER } from '../../../utils/validations';
import { MnemonicAutocompleteContainer } from './MnemonicAutocompleteContainer';
import { COLUMNS_COUNT } from './constants';
import * as styles from './MnemonicInput.scss';

const messages = defineMessages({
  recoveryPhraseNoResults: {
    id:
      'paper.wallet.create.certificate.verification.dialog.recoveryPhrase.noResults',
    defaultMessage: '!!!No results',
    description:
      '"Paper wallet create certificate verification dialog" recovery phrase no results label.',
  },
  mnemonicCounter: {
    id: 'paper.wallet.create.certificate.verification.dialog.mnemonicCounter',
    defaultMessage:
      '!!!{providedWordCount} of {requiredWordCount} words entered',
    description: 'Mnemonic input word counter',
  },
});

interface MnemonicInputProps {
  onChange?: (values: string[]) => void;

  value: string[];
  disabled?: boolean;
  error?: string;
  reset?: boolean;
  availableWords: string[];
  wordCount: number;
  label?: string;
}

const MnemonicInput: VFC<MnemonicInputProps> = injectIntl(
  ({
    intl,
    onChange,
    value: selectedWords,
    disabled,
    availableWords,
    wordCount,
    error,
    reset = false,
    label,
  }) => {
    useEffect(() => {
      if (selectedWords.length < 1) {
        onChange(times(wordCount, constant('')));
      }
    }, [selectedWords]);

    const providedWordCount = selectedWords.filter((word) => word?.length)
      .length;
    const requiredWordCount = selectedWords.length;
    const showError =
      providedWordCount === requiredWordCount &&
      error &&
      error !== INCOMPLETE_MNEMONIC_MARKER;
    const wordsPerColumn = Math.ceil(wordCount / COLUMNS_COUNT);
    const inputIndicesByColumnIndex = useMemo(
      () => chunk(times(wordCount), wordsPerColumn),
      [wordCount]
    );
    const inputRefs = times(wordCount, () => useRef<HTMLInputElement>());
    const createHandleWordChange = useCallback(
      (idx: number) => (newValue) => {
        if (newValue === selectedWords[idx]) return;

        const newSelectedWords = [...selectedWords];
        newSelectedWords[idx] = newValue;

        onChange(newSelectedWords);
      },
      [selectedWords, onChange]
    );
    const handleInputPaste = useCallback<
      ClipboardEventHandler<HTMLInputElement>
    >((event) => {
      // prevent input 'onChange' event
      event.preventDefault();
      const pastedWords = event.clipboardData.getData('Text').trim().split(' ');
      const filteredWords = pastedWords.filter((word) =>
        availableWords.includes(word)
      );
      // This is intentional, it should only be used for testing/development purposes.
      // If for whatever reason we decide to make it possible to paste the mnemonic
      // fragments, then this functionality is not implemented.
      if (filteredWords.length === wordCount) {
        onChange(filteredWords);
      }
    }, []);

    const createHandleConfirmSelection = useCallback(
      (idx: number) => () => {
        inputRefs[idx + 1]?.current.focus();
      },
      [inputRefs]
    );

    return (
      <div className={styles.root}>
        <div className={styles.header}>
          {label && (
            <div className={cx(styles.headerSlot, styles.headerLeftSlot)}>
              {label}
            </div>
          )}
          <div
            className={cx(
              styles.headerSlot,
              styles.headerRightSlot,
              showError && styles.headerError
            )}
          >
            {showError
              ? error
              : intl.formatMessage(messages.mnemonicCounter, {
                  providedWordCount,
                  requiredWordCount,
                })}
          </div>
        </div>
        <div className={styles.content}>
          {inputIndicesByColumnIndex.map((inputIndices) => (
            <div key={inputIndices.join('')} className={styles.inputList}>
              {inputIndices.map((idx) => {
                const value = selectedWords[idx] || '';
                return (
                  <div key={idx} className={styles.inputWrapper}>
                    <MnemonicAutocompleteContainer
                      ordinalNumber={idx + 1}
                      reset={reset}
                      options={availableWords}
                      value={value}
                      onChange={createHandleWordChange(idx)}
                      onConfirmSelection={createHandleConfirmSelection(idx)}
                      onPaste={handleInputPaste}
                      inputRef={inputRefs[idx]}
                      disabled={disabled}
                      error={error && !value}
                      maxVisibleOptions={5}
                      noResultsMessage={intl.formatMessage(
                        messages.recoveryPhraseNoResults
                      )}
                    />
                  </div>
                );
              })}
            </div>
          ))}
        </div>
      </div>
    );
  }
);

export { MnemonicInput };
