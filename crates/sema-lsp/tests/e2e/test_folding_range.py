"""Test textDocument/foldingRange."""

import pytest
from lsprotocol.types import (
    FoldingRangeParams,
    TextDocumentIdentifier,
)
from pytest_lsp import LanguageClient

from helpers import open_doc


@pytest.mark.asyncio
async def test_folding_range_multiline(client: LanguageClient):
    """Multi-line forms should produce folding ranges."""
    uri = await open_doc(
        client,
        "(defun foo (x)\n  (+ x 1))",
    )
    result = await client.text_document_folding_range_async(
        FoldingRangeParams(
            text_document=TextDocumentIdentifier(uri=uri),
        )
    )
    assert result is not None
    assert len(result) >= 1
    # The fold should start at line 0
    assert result[0].start_line == 0


@pytest.mark.asyncio
async def test_folding_range_single_line(client: LanguageClient):
    """Single-line forms should not produce folding ranges."""
    uri = await open_doc(client, "(define x 42)")
    result = await client.text_document_folding_range_async(
        FoldingRangeParams(
            text_document=TextDocumentIdentifier(uri=uri),
        )
    )
    assert result is not None
    assert len(result) == 0


@pytest.mark.asyncio
async def test_folding_range_nested(client: LanguageClient):
    """Nested multi-line forms should produce multiple fold ranges."""
    uri = await open_doc(
        client,
        "(defun foo (x)\n  (let ((y 1))\n    (+ x y)))",
    )
    result = await client.text_document_folding_range_async(
        FoldingRangeParams(
            text_document=TextDocumentIdentifier(uri=uri),
        )
    )
    assert result is not None
    assert len(result) >= 2  # outer defun + inner let
