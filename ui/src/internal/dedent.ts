/**
 * Remove common leading indentation from a multi-line string, so code authored
 * indented inside markup (e.g. slotted into `<sema-code>`) renders flush-left
 * while preserving *relative* indentation.
 *
 * Follows the TC39 `String.dedent` / Python `textwrap.dedent` approach:
 *  - drop a single leading newline (the one right after the opening tag),
 *  - drop fully-blank leading and trailing lines,
 *  - compute the minimum leading-whitespace width across non-blank lines,
 *  - strip that width from every line (blank lines collapse to empty).
 *
 * A line indented deeper than the common minimum keeps the extra indentation.
 *
 * @example
 * dedent("\n      (define x\n        42)\n    ")  // => "(define x\n  42)"
 */
export function dedent(input: string): string {
  if (!input) return '';

  // Normalize CRLF, drop a single leading newline.
  const lines = input.replace(/\r\n/g, '\n').replace(/^\n/, '').split('\n');

  // Trim blank lines from both ends (whitespace-only counts as blank).
  while (lines.length && lines[0].trim() === '') lines.shift();
  while (lines.length && lines[lines.length - 1].trim() === '') lines.pop();
  if (lines.length === 0) return '';

  // Minimum leading whitespace across non-blank lines.
  let min = Infinity;
  for (const line of lines) {
    if (line.trim() === '') continue; // blank lines don't constrain the minimum
    const indent = line.match(/^[ \t]*/)![0].length;
    if (indent < min) min = indent;
  }
  if (!Number.isFinite(min) || min === 0) return lines.join('\n');

  return lines.map((line) => line.slice(min)).join('\n');
}
