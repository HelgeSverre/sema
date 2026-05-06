---
outline: [2, 3]
---

# Notebook

Sema includes a Jupyter-inspired notebook interface for interactive development. Write code in cells, evaluate them individually or all at once, and see results inline — all in the browser.

## Quick Start

```bash
# Create a new notebook
sema notebook new my-notebook.sema-nb

# Open in the browser
sema notebook serve my-notebook.sema-nb
```

This starts a local server at `http://localhost:8888` with the notebook UI.

## Cell Types

### Code Cells

Code cells contain Sema expressions. Evaluate with **Shift+Enter** (run and advance) or **Cmd/Ctrl+Enter** (run and stay).

Cells share a persistent environment — definitions in earlier cells are visible in later ones:

```sema
;; Cell 1
(define greet (fn (name) (format "Hello, ~a!" name)))

;; Cell 2 — can use greet from Cell 1
(greet "Sema")  ;=> "Hello, Sema!"
```

Output from `println`, `display`, and `print` is captured and shown in the cell output area.

### Markdown Cells

Markdown cells render formatted text for documentation, section headers, and notes. Supports headings, bold, italic, inline code, code blocks, and lists.

Click rendered markdown to edit. Press **Shift+Enter** to re-render.

## Keyboard Shortcuts

| Shortcut         | Action                        |
| ---------------- | ----------------------------- |
| `Shift+Enter`    | Run cell and advance to next  |
| `Cmd/Ctrl+Enter` | Run cell and stay focused     |
| `Cmd/Ctrl+S`     | Save notebook                 |
| `Tab`            | Insert 2 spaces               |
| `Escape`         | Deselect cell                 |

## Toolbar

| Button          | Action                                      |
| --------------- | ------------------------------------------- |
| **+ Code**      | Add a new code cell at the end              |
| **+ Markdown**  | Add a new markdown cell at the end          |
| **Run All**     | Evaluate all code cells in order            |
| **Undo**        | Undo the last cell evaluation (restores environment) |
| **Save**        | Save the notebook to disk                   |
| **Reset**       | Clear all outputs and reset the environment |

You can also insert cells between existing ones by hovering between cells and clicking the **+** button that appears.

## Undo

After evaluating a cell, click **Undo** (or the inline "Undo cell" button on error outputs) to roll back:
- The cell's outputs are restored to their previous state
- The interpreter environment is rolled back to before the evaluation
- Downstream stale markers are reverted

This is useful when a cell modifies global state unexpectedly.

## File Format

Notebooks are saved as `.sema-nb` files in JSON format:

```json
{
  "version": 1,
  "metadata": {
    "title": "My Notebook",
    "created": "2026-01-01T00:00:00Z",
    "modified": "2026-01-01T12:00:00Z",
    "sema_version": "1.14.2"
  },
  "cells": [
    {
      "id": "c12345678",
      "type": "code",
      "source": "(+ 1 2)",
      "outputs": []
    }
  ]
}
```

## Headless Execution

Run all cells without starting the browser UI:

```bash
sema notebook run my-notebook.sema-nb
```

This evaluates all code cells in order, printing stdout to the terminal. Useful for CI validation or batch processing.

Run specific cells by index (1-based):

```bash
sema notebook run my-notebook.sema-nb --cells 1,3,5
```

## Export

Export a notebook to Markdown:

```bash
# To stdout
sema notebook export my-notebook.sema-nb

# To file
sema notebook export my-notebook.sema-nb -o output.md
```

The export includes code blocks with output, markdown sections, and error messages.

## CLI Reference

See [`sema notebook`](/docs/cli.html#sema-notebook) in the CLI reference for all flags and options.
