"""Test textDocument/definition."""

import pytest
from lsprotocol.types import (
    DefinitionParams,
    Location,
    Position,
    TextDocumentIdentifier,
)
from pytest_lsp import LanguageClient

from helpers import open_doc


@pytest.mark.asyncio
async def test_goto_definition(client: LanguageClient):
    """Go-to-definition on a usage should jump to the definition."""
    uri = await open_doc(client, "(defun foo (x) x)\n(foo 42)")
    result = await client.text_document_definition_async(
        DefinitionParams(
            text_document=TextDocumentIdentifier(uri=uri),
            position=Position(line=1, character=1),  # on 'foo' usage
        )
    )
    assert result is not None
    if isinstance(result, list):
        assert len(result) >= 1
        loc = result[0]
    else:
        loc = result
    # Definition should be on line 0
    assert loc.range.start.line == 0
