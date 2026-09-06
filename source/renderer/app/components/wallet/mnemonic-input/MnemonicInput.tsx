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
import styles from './MnemonicInput.scss';
import { Intl } from '../../../types/i18nTypes';

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

interface InteractiveMnemonicInputProps {
  onChange: (values: string[]) => void;

  value: string[];
  error?: string;
  reset?: boolean;
  availableWords: string[];
  wordCount: number;
  label?: string;
}

interface DisabledMnemonicInputProps {
  value: string[];
  wordCount: number;
  disabled: true;
}

const MnemonicInput: VFC<
  InteractiveMnemonicInputProps | DisabledMnemonicInputProps
> = injectIntl(
  ({
    intl,
    onChange,
    value: selectedWords,
    disabled,
    availableWords = [],
    wordCount,
    error,
    reset = false,
    label,
  }: InteractiveMnemonicInputProps &
    DisabledMnemonicInputProps & { intl: Intl }) => {
    useEffect(() => {
      if (selectedWords.length < 1) {
        onChange(times(wordCount, constant('')));
      }
    }, [selectedWords]);

    const providedWordCount = selectedWords.filter((word) => word?.length)
      .length;
    const showError =
      providedWordCount === wordCount &&
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
        if (!onChange || newValue === selectedWords[idx]) return;

        const newSelectedWords = [...selectedWords];
        newSelectedWords[idx] = newValue;

        onChange(newSelectedWords);
      },
      [selectedWords, onChange]
    );
    const handleInputPaste = useCallback<
      ClipboardEventHandler<HTMLInputElement>
    >(
      (event) => {
        const pastedWords = event.clipboardData
          .getData('Text')
          .trim()
          .split(' ');
        const filteredWords = pastedWords.filter((word) =>
          availableWords.includes(word)
        );

        // single word paste will be handled by `onChange` event
        // multiple word paste will be handled by `onPaste` event itself, and prevents `onChange` invocation
        if (filteredWords.length > 1) {
          event.preventDefault();
        }

        if (filteredWords.length === wordCount) {
          onChange(filteredWords);
        }
      },
      [availableWords, onChange]
    );

    const createHandleConfirmSelection = useCallback(
      (idx: number) => () => {
        inputRefs[idx + 1]?.current.focus();
      },
      [inputRefs]
    );

    return (
      <div className={styles.root}>
        {!disabled && (
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
                    requiredWordCount: wordCount,
                  })}
            </div>
          </div>
        )}
        <div className={styles.content}>
          {inputIndicesByColumnIndex.map((inputIndices) => (
            <div key={inputIndices.join('')} className={styles.inputList}>
              {inputIndices.map((idx) => {
                const value = selectedWords[idx];
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
