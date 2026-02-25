"""Test textDocument/hover."""

import pytest
from lsprotocol.types import (
    HoverParams,
    Position,
    TextDocumentIdentifier,
)
from pytest_lsp import LanguageClient

from helpers import open_doc


@pytest.mark.asyncio
async def test_hover_builtin(client: LanguageClient):
    """Hovering over a builtin like 'map' should return documentation."""
    uri = await open_doc(client, "(map inc (list 1 2 3))")
    result = await client.text_document_hover_async(
        HoverParams(
            text_document=TextDocumentIdentifier(uri=uri),
            position=Position(line=0, character=1),  # on 'map'
        )
    )
    assert result is not None
    assert result.contents is not None
    content = result.contents.value if hasattr(result.contents, "value") else str(result.contents)
    assert len(content) > 0


@pytest.mark.asyncio
async def test_hover_user_defined(client: LanguageClient):
    """Hovering over a user-defined function should show its signature."""
    uri = await open_doc(client, "(defun add (a b) (+ a b))\n(add 1 2)")
    result = await client.text_document_hover_async(
        HoverParams(
            text_document=TextDocumentIdentifier(uri=uri),
            position=Position(line=1, character=1),  # on 'add'
        )
    )
    assert result is not None
    content = result.contents.value if hasattr(result.contents, "value") else str(result.contents)
    assert "add" in content
    assert "User-defined" in content


@pytest.mark.asyncio
async def test_hover_special_form(client: LanguageClient):
    """Hovering over a special form like 'if' should identify it."""
    uri = await open_doc(client, "(if #t 1 0)")
    result = await client.text_document_hover_async(
        HoverParams(
            text_document=TextDocumentIdentifier(uri=uri),
            position=Position(line=0, character=1),  # on 'if'
        )
    )
    assert result is not None
    content = result.contents.value if hasattr(result.contents, "value") else str(result.contents)
    assert "if" in content


@pytest.mark.asyncio
async def test_hover_no_symbol(client: LanguageClient):
    """Hovering over whitespace should return None."""
    uri = await open_doc(client, "  (+ 1 2)")
    result = await client.text_document_hover_async(
        HoverParams(
            text_document=TextDocumentIdentifier(uri=uri),
            position=Position(line=0, character=0),  # whitespace
        )
    )
    assert result is None
