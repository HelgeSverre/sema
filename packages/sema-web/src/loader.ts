/**
 * Script loader for `<script type="text/sema">` tags.
 *
 * Discovers and evaluates Sema scripts embedded in HTML pages,
 * supporting both inline code and external `.sema` files via `src`.
 *
 * @module
 */

interface SemaInterpreterLike {
  evalStr(code: string): { value: string | null; output: string[]; error: string | null };
}

/** Options for the script loader. */
export interface LoaderOptions {
  /**
   * MIME type to match. Default: `"text/sema"`.
   * Scripts with `type` matching this value will be evaluated.
   */
  type?: string;
}

/**
 * Discover and evaluate all `<script type="text/sema">` tags in the document.
 *
 * Scripts are evaluated in document order:
 * 1. External scripts (`<script type="text/sema" src="app.sema">`) are fetched first
 * 2. Inline scripts (`<script type="text/sema">...</script>`) are evaluated directly
 *
 * Errors are logged to the console but do not halt execution of subsequent scripts.
 *
 * @param interp - The Sema interpreter to evaluate scripts with
 * @param opts - Loader options
 * @returns Array of results from each script evaluation
 */
export async function loadScripts(
  interp: SemaInterpreterLike,
  opts?: LoaderOptions,
): Promise<Array<{ value: string | null; output: string[]; error: string | null }>> {
  const mimeType = opts?.type ?? "text/sema";
  const scripts = document.querySelectorAll(`script[type="${mimeType}"]`);
  const results: Array<{ value: string | null; output: string[]; error: string | null }> = [];

  for (const script of scripts) {
    const src = script.getAttribute("src");
    let code: string;

    if (src) {
      try {
        const resp = await fetch(src);
        if (!resp.ok) {
          const err = `Failed to fetch ${src}: ${resp.status} ${resp.statusText}`;
          console.error(`[sema-web] ${err}`);
          results.push({ value: null, output: [], error: err });
          continue;
        }
        code = await resp.text();
      } catch (e) {
        const err = `Failed to fetch ${src}: ${e instanceof Error ? e.message : String(e)}`;
        console.error(`[sema-web] ${err}`);
        results.push({ value: null, output: [], error: err });
        continue;
      }
    } else {
      code = script.textContent ?? "";
    }

    if (!code.trim()) {
      results.push({ value: null, output: [], error: null });
      continue;
    }

    try {
      const result = interp.evalStr(code);

      // Log output lines to console
      for (const line of result.output) {
        console.log(`[sema] ${line}`);
      }

      if (result.error) {
        console.error(`[sema-web] Error in ${src ?? "inline script"}: ${result.error}`);
      }

      results.push(result);
    } catch (e) {
      const err = `Evaluation error: ${e instanceof Error ? e.message : String(e)}`;
      console.error(`[sema-web] ${err}`);
      results.push({ value: null, output: [], error: err });
    }
  }

  return results;
}
