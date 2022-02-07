import React from 'react';
import { text } from '@storybook/addon-knobs';
import NoDiskSpaceError from '../../../../source/renderer/app/components/loading/no-disk-space-error/NoDiskSpaceError';

export const NoDiskSpaceErrorStory = () => (
  <NoDiskSpaceError
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'number' is not assignable to par... Remove this comment to see the full error message
    diskSpaceRequired={`${text('diskSpaceRequired (GB)', 4)} GB`}
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'number' is not assignable to par... Remove this comment to see the full error message
    diskSpaceMissing={`${text('diskSpaceRequired (GB)', 1)} GB`}
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'number' is not assignable to par... Remove this comment to see the full error message
    diskSpaceRecommended={`${text('diskSpaceRequired (GB)', 8)} GB`}
  />
);
