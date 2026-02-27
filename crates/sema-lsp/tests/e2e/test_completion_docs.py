"""Test that completion items include documentation."""

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
async def test_completion_builtin_has_documentation(client: LanguageClient):
    """Completion items for builtins should include documentation."""
    uri = await open_doc(client, "(ma")
    result = await client.text_document_completion_async(
        CompletionParams(
            text_document=TextDocumentIdentifier(uri=uri),
            position=Position(line=0, character=3),  # after 'ma'
        )
    )
    assert result is not None
    items = result.items if isinstance(result, CompletionList) else result
    map_items = [i for i in items if i.label == "map"]
    assert len(map_items) >= 1
    item = map_items[0]
    assert item.documentation is not None


@pytest.mark.asyncio
async def test_completion_special_form_has_documentation(client: LanguageClient):
    """Completion items for special forms should include documentation."""
    uri = await open_doc(client, "(def")
    result = await client.text_document_completion_async(
        CompletionParams(
            text_document=TextDocumentIdentifier(uri=uri),
            position=Position(line=0, character=4),
        )
    )
    assert result is not None
    items = result.items if isinstance(result, CompletionList) else result
    define_items = [i for i in items if i.label == "define"]
    assert len(define_items) >= 1
    item = define_items[0]
    assert item.documentation is not None
