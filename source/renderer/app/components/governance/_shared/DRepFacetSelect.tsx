import React from 'react';
import { Select } from 'react-polymorph/lib/components/Select';
import { SelectSkin } from 'react-polymorph/lib/skins/simple/SelectSkin';
import styles from './DRepFacetSelect.scss';

interface Props {
  label: string;
  value: string;
  options: Array<[string, string]>;
  onChange: (next: string) => void;
}

/**
 * One labelled dropdown in a governance filter or criteria row.
 *
 * The app's own select rather than a native one dressed down to look like
 * ours. Every other dropdown in Daedalus is this component at this height,
 * and a row of controls that matches nothing else on screen reads as
 * unfinished however carefully it is styled. The label is the select's own,
 * not a caption placed above it, so it lines up with the settings screen
 * without a second layout rule saying how.
 */
function DRepFacetSelect({ label, value, options, onChange }: Props) {
  return (
    <Select
      className={styles.facet}
      label={label}
      value={value}
      options={options.map(([optionValue, optionLabel]) => ({
        value: optionValue,
        label: optionLabel,
      }))}
      onChange={onChange}
      skin={SelectSkin}
      optionHeight={50}
    />
  );
}

export default DRepFacetSelect;
