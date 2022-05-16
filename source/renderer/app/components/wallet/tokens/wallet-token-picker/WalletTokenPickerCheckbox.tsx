import React, { useCallback } from 'react';
import { injectIntl } from 'react-intl';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { messages } from './WalletTokenPicker.messages';
import { MAX_TOKENS } from './const';
import type { ItemProps as Props } from './types';

function WalletTokenPickerCheckbox({
  intl,
  className,
  isChecked,
  isMaxCount,
  isPreviouslyChecked,
  uniqueId,
  toggleCheckbox,
}: Props) {
  const checked = isChecked || isPreviouslyChecked;
  const toolTipDisabled = !isMaxCount || checked;
  const onChange = useCallback(() => toggleCheckbox(uniqueId), [
    toggleCheckbox,
    uniqueId,
  ]);
  return (
    <div className={className}>
      <PopOver
        maxWidth={315}
        disabled={toolTipDisabled}
        content={intl.formatMessage(messages.maxTokensWarning, {
          maxTokens: MAX_TOKENS,
        })}
      >
        <Checkbox
          checked={checked}
          onChange={onChange}
          disabled={isPreviouslyChecked}
        />
      </PopOver>
    </div>
  );
}

export default injectIntl(WalletTokenPickerCheckbox);
