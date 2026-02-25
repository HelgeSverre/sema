"""Test that the server advertises expected capabilities."""

import pytest
from pytest_lsp import LanguageClient


@pytest.mark.asyncio
async def test_server_capabilities(client: LanguageClient):
    """Server should advertise all expected capabilities."""
    caps = client.server_capabilities
    assert caps is not None

    assert caps.text_document_sync is not None
    assert caps.completion_provider is not None
    assert caps.hover_provider is not None
    assert caps.definition_provider is not None
    assert caps.references_provider is not None
    assert caps.rename_provider is not None
    assert caps.document_symbol_provider is not None
    assert caps.code_lens_provider is not None
    assert caps.signature_help_provider is not None
