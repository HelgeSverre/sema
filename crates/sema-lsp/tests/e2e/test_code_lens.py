"""Test textDocument/codeLens."""

import pytest
from lsprotocol.types import (
    CodeLensParams,
    TextDocumentIdentifier,
)
from pytest_lsp import LanguageClient

from helpers import open_doc


@pytest.mark.asyncio
async def test_code_lens_top_level_forms(client: LanguageClient):
    """Each top-level form should get a Run code lens."""
    uri = await open_doc(
        client,
        "(define x 1)\n(+ x 2)",
    )
    result = await client.text_document_code_lens_async(
        CodeLensParams(
            text_document=TextDocumentIdentifier(uri=uri),
        )
    )
    assert result is not None
    assert len(result) >= 2
    for lens in result:
        assert lens.command is not None
        assert lens.command.command == "sema.runTopLevel"


@pytest.mark.asyncio
async def test_code_lens_empty_doc(client: LanguageClient):
    """Empty document should produce no code lenses."""
    uri = await open_doc(client, "")
    result = await client.text_document_code_lens_async(
        CodeLensParams(
            text_document=TextDocumentIdentifier(uri=uri),
        )
    )
    assert result is None or len(result) == 0


@pytest.mark.asyncio
async def test_code_lens_has_uri_and_index(client: LanguageClient):
    """Code lens arguments should include uri and formIndex."""
    uri = await open_doc(client, "(+ 1 2)")
    result = await client.text_document_code_lens_async(
        CodeLensParams(
            text_document=TextDocumentIdentifier(uri=uri),
        )
    )
    assert result is not None
    assert len(result) >= 1
    args = result[0].command.arguments
    assert args is not None
    assert len(args) >= 1
    assert "uri" in args[0]
    assert "formIndex" in args[0]
