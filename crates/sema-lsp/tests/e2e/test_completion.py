"""Test textDocument/completion."""

import pytest
from lsprotocol.types import (
    CompletionList,
    CompletionParams,
    Position,
    TextDocumentIdentifier,
)
from pytest_lsp import LanguageClient

from helpers import open_doc


@pytest.mark.asyncio
async def test_completion_returns_items(client: LanguageClient):
    """Completion should return builtin and user-defined symbols."""
    uri = await open_doc(client, "(define my-var 42)\n(")
    result = await client.text_document_completion_async(
        CompletionParams(
            text_document=TextDocumentIdentifier(uri=uri),
            position=Position(line=1, character=1),  # after '('
        )
    )
    assert result is not None
    items = result.items if isinstance(result, CompletionList) else result
    labels = [item.label for item in items]
    # Should include builtins
    assert any("define" in l for l in labels)
    # Should include user-defined symbol
    assert "my-var" in labels


@pytest.mark.asyncio
async def test_completion_includes_special_forms(client: LanguageClient):
    """Completion should include special forms like if, let, lambda."""
    uri = await open_doc(client, "(")
    result = await client.text_document_completion_async(
        CompletionParams(
            text_document=TextDocumentIdentifier(uri=uri),
            position=Position(line=0, character=1),
        )
    )
    assert result is not None
    items = result.items if isinstance(result, CompletionList) else result
    labels = [item.label for item in items]
    assert "if" in labels
    assert "lambda" in labels
    assert "let" in labels
