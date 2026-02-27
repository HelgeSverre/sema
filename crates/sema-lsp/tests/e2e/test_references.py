"""Test textDocument/references."""

import pytest
from lsprotocol.types import (
    Position,
    ReferenceContext,
    ReferenceParams,
    TextDocumentIdentifier,
)
from pytest_lsp import LanguageClient

from helpers import open_doc


@pytest.mark.asyncio
async def test_references_variable(client: LanguageClient):
    """Find references should return all occurrences of a variable."""
    uri = await open_doc(client, "(define x 42)\n(+ x 1)\n(* x 2)")
    result = await client.text_document_references_async(
        ReferenceParams(
            text_document=TextDocumentIdentifier(uri=uri),
            position=Position(line=0, character=8),  # on 'x'
            context=ReferenceContext(include_declaration=True),
        )
    )
    assert result is not None
    assert len(result) >= 3  # definition + 2 usages


@pytest.mark.asyncio
async def test_references_function(client: LanguageClient):
    """Find references for a function should include definition and calls."""
    uri = await open_doc(
        client,
        "(defun foo (x) x)\n(foo 1)\n(foo 2)",
    )
    result = await client.text_document_references_async(
        ReferenceParams(
            text_document=TextDocumentIdentifier(uri=uri),
            position=Position(line=1, character=1),  # on 'foo'
            context=ReferenceContext(include_declaration=True),
        )
    )
    assert result is not None
    assert len(result) >= 3  # definition + 2 calls


@pytest.mark.asyncio
async def test_references_local_only(client: LanguageClient):
    """References for a local variable should not leak to other scopes."""
    uri = await open_doc(
        client,
        "(define x 1)\n(let ((x 2)) (+ x 1))\n(+ x 3)",
    )
    result = await client.text_document_references_async(
        ReferenceParams(
            text_document=TextDocumentIdentifier(uri=uri),
            position=Position(line=1, character=16),  # on 'x' in let body
            context=ReferenceContext(include_declaration=True),
        )
    )
    assert result is not None
    # Should only include the let-bound x and its usage, not the top-level x
    assert len(result) == 2


@pytest.mark.asyncio
async def test_references_no_symbol(client: LanguageClient):
    """References on whitespace should return None."""
    uri = await open_doc(client, "  (+ 1 2)")
    result = await client.text_document_references_async(
        ReferenceParams(
            text_document=TextDocumentIdentifier(uri=uri),
            position=Position(line=0, character=0),
            context=ReferenceContext(include_declaration=True),
        )
    )
    assert result is None
