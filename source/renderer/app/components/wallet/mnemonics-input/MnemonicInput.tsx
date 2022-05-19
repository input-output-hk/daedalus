import React, { useCallback, useEffect, useMemo, VFC } from 'react';
import { injectIntl, defineMessages } from 'react-intl';
import { chunk, constant, times } from 'lodash';
import { MnemonicsAutocompleteContainer } from './MnemonicAutocompleteContainer';
import * as styles from './MnemonicInput.scss';
import { COLUMNS_COUNT } from './constants';

const messages = defineMessages({
  recoveryPhraseNoResults: {
    id:
      'paper.wallet.create.certificate.verification.dialog.recoveryPhrase.noResults',
    defaultMessage: '!!!No results',
    description:
      '"Paper wallet create certificate verification dialog" recovery phrase no results label.',
  },
});

interface MnemonicsInputProps {
  onChange?: (values: string[]) => void;

  value: string[];
  disabled?: boolean;
  error: boolean;
  reset: boolean;
  availableWords: string[];
  wordsCount: number;
}

const MnemonicInput: VFC<MnemonicsInputProps> = injectIntl(
  ({
    intl,
    onChange,
    value: selectedWords,
    disabled,
    availableWords,
    wordsCount,
    error,
    reset,
  }) => {
    useEffect(() => {
      if (selectedWords.length < 1) {
        onChange(times(wordsCount, constant('')));
      }
    }, [selectedWords]);

    const createHandleWordChange = useCallback(
      (idx: number) => (newValue) => {
        if (newValue === selectedWords[idx]) return;

        const newSelectedWords = [...selectedWords];
        newSelectedWords[idx] = newValue;

        onChange(newSelectedWords);
      },
      [selectedWords, onChange]
    );
    const wordsPerColumn = Math.ceil(wordsCount / COLUMNS_COUNT);
    const inputIndicesByColumnIndex = useMemo(
      () => chunk(times(wordsCount), wordsPerColumn),
      [wordsCount]
    );

    return (
      <div className={styles.root}>
        {inputIndicesByColumnIndex.map((inputIndices) => (
          <div key={inputIndices.join('')} className={styles.inputList}>
            {inputIndices.map((idx) => {
              const value = selectedWords[idx] || '';
              return (
                <div key={idx} className={styles.inputWrapper}>
                  <MnemonicsAutocompleteContainer
                    ordinalNumber={idx + 1}
                    reset={reset}
                    options={availableWords}
                    value={value}
                    onChange={createHandleWordChange(idx)}
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
    );
  }
);

export { MnemonicInput };
