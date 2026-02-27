"""Test textDocument/inlayHint."""

import pytest
from lsprotocol.types import (
    InlayHintParams,
    Position,
    Range,
    TextDocumentIdentifier,
)
from pytest_lsp import LanguageClient

from helpers import open_doc


@pytest.mark.asyncio
async def test_inlay_hints_user_function(client: LanguageClient):
    """Inlay hints should show parameter names for user-defined functions."""
    uri = await open_doc(client, "(defun add (a b) (+ a b))\n(add 1 2)")
    result = await client.text_document_inlay_hint_async(
        InlayHintParams(
            text_document=TextDocumentIdentifier(uri=uri),
            range=Range(
                start=Position(line=0, character=0),
                end=Position(line=1, character=20),
            ),
        )
    )
    assert result is not None
    assert len(result) >= 2
    labels = [h.label for h in result]
    assert "a:" in labels
    assert "b:" in labels


@pytest.mark.asyncio
async def test_inlay_hints_no_hints_for_special_forms(client: LanguageClient):
    """Special forms like define/if should not get inlay hints."""
    uri = await open_doc(client, "(define x 42)\n(if #t 1 0)")
    result = await client.text_document_inlay_hint_async(
        InlayHintParams(
            text_document=TextDocumentIdentifier(uri=uri),
            range=Range(
                start=Position(line=0, character=0),
                end=Position(line=1, character=20),
            ),
        )
    )
    # Should be None or empty — no hints for special forms
    assert result is None or len(result) == 0


@pytest.mark.asyncio
async def test_inlay_hints_empty_doc(client: LanguageClient):
    """Empty document should return no hints."""
    uri = await open_doc(client, "")
    result = await client.text_document_inlay_hint_async(
        InlayHintParams(
            text_document=TextDocumentIdentifier(uri=uri),
            range=Range(
                start=Position(line=0, character=0),
                end=Position(line=0, character=0),
            ),
        )
    )
    assert result is None or len(result) == 0
