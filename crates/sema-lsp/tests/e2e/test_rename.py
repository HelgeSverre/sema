"""Test textDocument/rename and textDocument/prepareRename."""

import pytest
from lsprotocol.types import (
    Position,
    PrepareRenameParams,
    RenameParams,
    TextDocumentIdentifier,
)
from pytest_lsp import LanguageClient

from helpers import open_doc


@pytest.mark.asyncio
async def test_prepare_rename_user_symbol(client: LanguageClient):
    """prepareRename on a user-defined symbol should return its range."""
    uri = await open_doc(client, "(define x 42)\n(+ x 1)")
    result = await client.text_document_prepare_rename_async(
        PrepareRenameParams(
            text_document=TextDocumentIdentifier(uri=uri),
            position=Position(line=0, character=8),  # on 'x'
        )
    )
    assert result is not None


@pytest.mark.asyncio
async def test_prepare_rename_builtin_rejected(client: LanguageClient):
    """prepareRename on a builtin should return None (can't rename builtins)."""
    uri = await open_doc(client, "(map inc (list 1 2 3))")
    result = await client.text_document_prepare_rename_async(
        PrepareRenameParams(
            text_document=TextDocumentIdentifier(uri=uri),
            position=Position(line=0, character=1),  # on 'map'
        )
    )
    assert result is None


@pytest.mark.asyncio
async def test_rename_user_variable(client: LanguageClient):
    """Renaming a user variable should produce edits for all occurrences."""
    uri = await open_doc(client, "(define x 42)\n(+ x 1)")
    result = await client.text_document_rename_async(
        RenameParams(
            text_document=TextDocumentIdentifier(uri=uri),
            position=Position(line=0, character=8),  # on 'x'
            new_name="value",
        )
    )
    assert result is not None
    assert result.changes is not None
    edits = result.changes.get(uri, [])
    # Should rename both the definition and the usage
    assert len(edits) >= 2
    for edit in edits:
        assert edit.new_text == "value"
