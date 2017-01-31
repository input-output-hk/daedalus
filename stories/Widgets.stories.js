import React from 'react';
import { storiesOf } from '@kadira/storybook';
import StoryDecorator from './support/StoryDecorator';
import CheckboxWithLongLabel from '../app/components/widgets/forms/CheckboxWithLongLabel';
import BigButtonForDialogs from '../app/components/widgets/BigButtonForDialogs';
import createIcon from '../app/assets/images/create-ic.svg';
import importIcon from '../app/assets/images/import-ic.svg';
import joinSharedIcon from '../app/assets/images/join-shared-ic.svg';

storiesOf('Widgets', module)

  .addDecorator((story) => (
    <StoryDecorator>
      {story()}
    </StoryDecorator>
  ))

  // ====== Stories ======

  .add('CheckboxWithLongLabel - checked', () => (
    <div>
      <CheckboxWithLongLabel
        label={`I understand that if this application is moved to another device or deleted,
        my money can be only recovered with the backup phrase which were written down in a secure place`}
        checked={true}
        onChange={() => {}}
      />
    </div>
  ))

  .add('CheckboxWithLongLabel - unchecked', () => (
    <div>
      <CheckboxWithLongLabel
        label={`I understand that if this application is moved to another device or deleted,
        my money can be only recovered with the backup phrase which were written down in a secure place`}
        checked={false}
        onChange={() => {}}
      />
    </div>
  ))

  .add('BigButtonForDialogs', () => (
    <div>
      <div style={{ width: '300px', height: '200px', display: 'flex' }}>
        <BigButtonForDialogs
          description="Create new wallet"
          label="Create"
          icon={createIcon}
          onClick={() => {}}
        />
      </div>
      <div style={{ width: '300px', height: '200px', display: 'flex' }}>
        <BigButtonForDialogs
          description="Join shared wallet up to 5 people"
          label="Join"
          icon={joinSharedIcon}
          onClick={() => {}}
          isDisabled
        />
      </div>
      <div style={{ width: '300px', height: '200px', display: 'flex' }}>
        <BigButtonForDialogs
          description="Import existing wallet"
          label="Import"
          icon={importIcon}
          onClick={() => {}}
        />
      </div>
    </div>
  ));
