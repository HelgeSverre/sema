"""Test textDocument/semanticTokens/full."""

import pytest
from lsprotocol.types import (
    SemanticTokensParams,
    TextDocumentIdentifier,
)
from pytest_lsp import LanguageClient

from helpers import open_doc


@pytest.mark.asyncio
async def test_semantic_tokens_returns_data(client: LanguageClient):
    """Semantic tokens should return token data for a document."""
    uri = await open_doc(client, "(define x 42)\n(defun foo (a) (+ a x))")
    result = await client.text_document_semantic_tokens_full_async(
        SemanticTokensParams(
            text_document=TextDocumentIdentifier(uri=uri),
        )
    )
    assert result is not None
    assert len(result.data) > 0
    # Data comes in groups of 5 (deltaLine, deltaStart, length, tokenType, modifiers)
    assert len(result.data) % 5 == 0


@pytest.mark.asyncio
async def test_semantic_tokens_classifies_keyword(client: LanguageClient):
    """Special forms should be classified as keyword (type index 0)."""
    uri = await open_doc(client, "(define x 42)")
    result = await client.text_document_semantic_tokens_full_async(
        SemanticTokensParams(
            text_document=TextDocumentIdentifier(uri=uri),
        )
    )
    assert result is not None
    # First token should be 'define' at (0,1) — keyword type = 0
    tokens = result.data
    assert len(tokens) >= 5
    # tokenType is at index 3 in each group of 5
    assert tokens[3] == 0  # KEYWORD


@pytest.mark.asyncio
async def test_semantic_tokens_empty_doc(client: LanguageClient):
    """Empty document should return None or empty tokens."""
    uri = await open_doc(client, "")
    result = await client.text_document_semantic_tokens_full_async(
        SemanticTokensParams(
            text_document=TextDocumentIdentifier(uri=uri),
        )
    )
    # Either None or empty data is acceptable
    if result is not None:
        assert len(result.data) == 0
