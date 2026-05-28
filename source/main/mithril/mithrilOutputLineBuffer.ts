export type MithrilBufferedLineHandler = (line: string) => void;

export class MithrilOutputLineBuffer {
  _buffer = '';
  _handleLine: MithrilBufferedLineHandler;

  constructor(handleLine: MithrilBufferedLineHandler) {
    this._handleLine = handleLine;
  }

  push(chunk: string): void {
    this._buffer += chunk;
    const lines = this._buffer.split('\n');
    this._buffer = lines.pop() || '';
    lines.forEach(this._handleLine);
  }

  flush(): void {
    if (!this._buffer.trim()) return;
    this._handleLine(this._buffer);
    this._buffer = '';
  }
}
