// Type-only imports: sema-menu/sema-tree import these detail types back, so any
// runtime import here would create a cycle.
import type { SemaMenuItem } from './sema-menu.js';
import type { SemaTreeItem } from './sema-tree.js';

/** Detail of the `sema-select` event emitted by `<sema-menu>`. */
export interface SemaSelectEventDetail {
  value: string;
  item: SemaMenuItem;
}

/** Detail of the `sema-change` event emitted by `<sema-toggle-group>` and `<sema-tabs>`. */
export interface SemaChangeEventDetail {
  value: string;
}

/** Detail of the `sema-tree-select` event emitted by `<sema-tree-item>`. */
export interface SemaTreeSelectEventDetail {
  label?: string;
  element: SemaTreeItem;
}

/** Detail of the `sema-page-change` event emitted by `<sema-pagination>`. */
export interface SemaPageChangeEventDetail {
  /** The newly selected page (1-based). */
  page: number;
}

/** Detail of the `sema-resize` event emitted by `<sema-splitter>`. */
export interface SemaResizeEventDetail {
  /** Pixel offset from the drag start, or the target size when `absolute` is set. */
  delta: number;
  /** Set on Home/End: `delta` is an absolute size (min/max), not an offset. */
  absolute?: boolean;
  /** Set when the resize came from arrow-key stepping. */
  keyboard?: boolean;
}

declare global {
  interface HTMLElementEventMap {
    'sema-select': CustomEvent<SemaSelectEventDetail>;
    'sema-change': CustomEvent<SemaChangeEventDetail>;
    'sema-tree-select': CustomEvent<SemaTreeSelectEventDetail>;
    'sema-page-change': CustomEvent<SemaPageChangeEventDetail>;
    'sema-resize': CustomEvent<SemaResizeEventDetail>;
    'sema-open': Event;
    'sema-close': Event;
    'sema-dialog-open': Event;
    'sema-dialog-close': Event;
    'sema-drawer-open': Event;
    'sema-drawer-close': Event;
    'sema-dismiss': Event;
    'sema-resize-start': Event;
    'sema-resize-end': Event;
  }
}
