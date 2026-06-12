import '../lib/sema-toaster.js';
import type { SemaToaster, ToastOptions, ToastHandle } from '../lib/sema-toaster.js';

/**
 * Imperative toast API. Call `toast('Saved')` from anywhere — it lazily finds-or-creates
 * a singleton `<sema-toaster>` appended to `document.body` (or reuses one you placed in
 * the DOM to control position). Variant shortcuts and `dismissAll()` included.
 *
 * ```ts
 * import { toast } from '@sema/ui';
 * toast.success('Published v1.2.0');
 * const t = toast('Uploading…', { duration: null });
 * t.update('Done', { variant: 'success', duration: 3000 });
 * ```
 */
function toaster(): SemaToaster {
  const existing = document.querySelector('sema-toaster') as SemaToaster | null;
  if (existing) return existing;
  const el = document.createElement('sema-toaster') as SemaToaster;
  document.body.appendChild(el);
  return el;
}

type ToastVariantFn = (message: string, opts?: Omit<ToastOptions, 'variant'>) => ToastHandle;

interface ToastApi {
  (message: string, opts?: ToastOptions): ToastHandle;
  success: ToastVariantFn;
  error: ToastVariantFn;
  warning: ToastVariantFn;
  info: ToastVariantFn;
  dismissAll(): void;
}

export const toast: ToastApi = Object.assign(
  (message: string, opts?: ToastOptions): ToastHandle => toaster().show(message, opts),
  {
    success: (m: string, o?: Omit<ToastOptions, 'variant'>) => toaster().show(m, { ...o, variant: 'success' }),
    error: (m: string, o?: Omit<ToastOptions, 'variant'>) => toaster().show(m, { ...o, variant: 'error' }),
    warning: (m: string, o?: Omit<ToastOptions, 'variant'>) => toaster().show(m, { ...o, variant: 'warning' }),
    info: (m: string, o?: Omit<ToastOptions, 'variant'>) => toaster().show(m, { ...o, variant: 'info' }),
    dismissAll: () => {
      (document.querySelector('sema-toaster') as SemaToaster | null)?.dismissAll();
    },
  },
);
