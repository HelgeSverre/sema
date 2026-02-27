"""Test textDocument/documentHighlight."""

import pytest
from lsprotocol.types import (
    DocumentHighlightParams,
    Position,
    TextDocumentIdentifier,
)
from pytest_lsp import LanguageClient

from helpers import open_doc


@pytest.mark.asyncio
async def test_highlight_variable_occurrences(client: LanguageClient):
    """Highlighting a variable should mark all occurrences."""
    uri = await open_doc(client, "(define x 42)\n(+ x 1)")
    result = await client.text_document_document_highlight_async(
        DocumentHighlightParams(
            text_document=TextDocumentIdentifier(uri=uri),
            position=Position(line=0, character=8),  # on 'x'
        )
    )
    assert result is not None
    assert len(result) >= 2  # definition + usage


@pytest.mark.asyncio
async def test_highlight_local_only(client: LanguageClient):
    """Highlighting a local variable should not include shadowed occurrences."""
    uri = await open_doc(
        client,
        "(define x 1)\n(let ((x 2)) x)",
    )
    result = await client.text_document_document_highlight_async(
        DocumentHighlightParams(
            text_document=TextDocumentIdentifier(uri=uri),
            position=Position(line=1, character=13),  # on 'x' in let body
        )
    )
    assert result is not None
    # Should only highlight the let-bound x and its usage, not the top-level x
    assert len(result) == 2


@pytest.mark.asyncio
async def test_highlight_builtin(client: LanguageClient):
    """Highlighting a builtin should return all occurrences in the document."""
    uri = await open_doc(client, "(map inc (list 1 2))\n(map dec (list 3 4))")
    result = await client.text_document_document_highlight_async(
        DocumentHighlightParams(
            text_document=TextDocumentIdentifier(uri=uri),
            position=Position(line=0, character=1),  # on 'map'
        )
    )
    assert result is not None
    assert len(result) == 2


@pytest.mark.asyncio
async def test_highlight_no_symbol(client: LanguageClient):
    """Highlighting whitespace should return None."""
    uri = await open_doc(client, "  (+ 1 2)")
    result = await client.text_document_document_highlight_async(
        DocumentHighlightParams(
            text_document=TextDocumentIdentifier(uri=uri),
            position=Position(line=0, character=0),
        )
    )
    assert result is None or len(result) == 0
