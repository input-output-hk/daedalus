import React from 'react';
import { render } from '@testing-library/react';
import IncidentOverlay from './IncidentOverlay';

describe('remote incident content', () => {
  it('renders Markdown but does not create elements from raw HTML', () => {
    const content =
      '**Important**\n\n<img src="x" onerror="alert(1)">\n\n<iframe src="https://example.com"></iframe>';
    const { container, getByText } = render(
      <IncidentOverlay
        incident={
          { content, title: 'Incident', date: '2026-10-19', action: {} } as any
        }
        onOpenExternalLink={jest.fn()}
        onProceedNewsAction={jest.fn()}
        currentDateFormat="YYYY-MM-DD"
      />
    );
    expect(getByText('Important').tagName).toBe('STRONG');
    expect(container.querySelector('img')).toBeNull();
    expect(container.querySelector('iframe')).toBeNull();
  });
});
