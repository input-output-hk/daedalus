import React from 'react';
import { storiesOf } from '@storybook/react';
import { select, withKnobs } from '@storybook/addon-knobs';
import { action } from '@storybook/addon-actions';
import StoryDecorator from '../_support/StoryDecorator';
import IncidentOverlay from '../../../source/renderer/app/components/news/IncidentOverlay';
import { dateOptions } from '../_support/profileSettings';
import { DATE_ENGLISH_OPTIONS } from '../../../source/renderer/app/config/profileConfig';

storiesOf('News|Overlays', module)
  .addDecorator((story) => (
    <StoryDecorator>
      {/* @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ action: { label: string; url: ... Remove this comment to see the full error message */}
      {story({
        action: {
          label: 'Read More',
          url: 'https://www.daedalus.io',
        },
        content:
          '# h1 Heading\nUt consequat semper viverra nam libero justo laoreet sit. Sagittis vitae et leo duis. Eget nullam non nisi est sit amet facilisis magna etiam. Nisl tincidunt eget nullam non nisi est sit amet facilisis. Auctor neque vitae tempus quam pellentesque. Vel facilisis volutpat est velit egestas dui id ornare arcu.\n\n## h2 Heading\n\nConsequat mauris nunc congue nisi vitae suscipit. Dictum non consectetur a erat nam. Laoreet non curabitur gravida arcu ac tortor dignissim. Eu augue ut lectus arcu bibendum at. Facilisis gravida neque convallis a cras semper. Ut consequat semper viverra nam libero justo laoreet sit. Sagittis vitae et leo duis. Eget nullam non nisi est sit amet facilisis magna etiam. Nisl tincidunt eget nullam non nisi est sit amet facilisis. Auctor neque vitae tempus quam pellentesque. Vel facilisis volutpat est velit egestas dui id ornare arcu. Nam aliquam sem et tortor consequat id porta nibh venenatis.\n\nViverra nam libero justo laoreet sit amet. Pharetra diam sit amet nisl. Quam viverra orci sagittis eu. Rhoncus dolor purus non enim. Posuere urna nec tincidunt praesent semper feugiat. Suspendisse in est ante in nibh mauris cursus. Sit amet consectetur adipiscing elit duis. Tortor id aliquet lectus proin nibh nisl condimentum id. At in tellus integer feugiat scelerisque. Maecenas sed enim ut sem viverra aliquet. Pellentesque pulvinar pellentesque habitant morbi. Ultrices neque ornare aenean euismod elementum nisi quis eleifend. Praesent tristique magna sit amet purus gravida. Diam volutpat commodo sed egestas egestas. Ut placerat orci nulla pellentesque dignissim enim. Ultrices in iaculis nunc sed augue lacus viverra. Etiam sit amet nisl purus.\n\n## Typographic replacements\n\nEnable typographer option to see result.\n\n(c) (C) (r) (R) (tm) (TM) (p) (P) +-\n\ntest.. test... test..... test?..... test!....\n\n!!!!!! ???? ,,  -- ---\n\n"Smartypants, double quotes" and \'single quotes\'\n\n## Emphasis\n\n**This is bold text**\n\n__This is bold text__\n\n*This is italic text*\n\n_This is italic text_\n\n## Lists\n\nUnordered\n\n+ Create a list by starting a line with +, -, or *\n+ Sub-lists are made by indenting 2 spaces:\n+ Very easy!\n\nOrdered\n\n1. Lorem ipsum dolor sit amet\n2. Consectetur adipiscing elit\n3. Integer molestie lorem at massa\n\n\n1. You can use sequential numbers...\n1. ...or keep all the numbers as `1.`\n\n## Links\n\n[link text](http://dev.nodeca.com)\n\n[link with title](http://nodeca.github.io/pica/demo/ "title text!")\n\nAutoconverted link https://github.com/nodeca/pica (enable linkify to see)\n\n### [Subscript](https://github.com/markdown-it/markdown-it-sub) / [Superscript](https://github.com/markdown-it/markdown-it-sup)\n\n- 19^th^\n- H~2~O\n',
        date: Date.now(),
        target: {
          daedalus: 'v0.13',
          platform: 'macOS',
          platformVersion: '10.14.6',
        },
        title: 'Lazarus Incident',
      })}
    </StoryDecorator>
  ))
  .addDecorator(withKnobs)
  .add('Incident Overlay', (props) => (
    <IncidentOverlay
      incident={props}
      onOpenExternalLink={action('onOpenExternalLink')}
      onProceedNewsAction={action('onProceedNewsAction')}
      currentDateFormat={select(
        'currentDateFormat',
        dateOptions,
        DATE_ENGLISH_OPTIONS[0].value
      )}
    />
  ))
  .add('Incident - Themed', (props) => (
    <IncidentOverlay
      incident={{ ...props, color: 'theme-default' }}
      onOpenExternalLink={action('onOpenExternalLink')}
      onProceedNewsAction={action('onProceedNewsAction')}
      currentDateFormat={select(
        'currentDateFormat',
        dateOptions,
        DATE_ENGLISH_OPTIONS[0].value
      )}
    />
  ))
  .add('Incident - Grey', (props) => (
    <IncidentOverlay
      incident={{ ...props, color: 'grey' }}
      onOpenExternalLink={action('onOpenExternalLink')}
      onProceedNewsAction={action('onProceedNewsAction')}
      currentDateFormat={select(
        'currentDateFormat',
        dateOptions,
        DATE_ENGLISH_OPTIONS[0].value
      )}
    />
  ));
