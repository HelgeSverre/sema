"""Test diagnostics (parse errors and compile warnings)."""

import pytest
from lsprotocol.types import DiagnosticSeverity
from pytest_lsp import LanguageClient

from helpers import open_doc


@pytest.mark.asyncio
async def test_valid_code_no_diagnostics(client: LanguageClient):
    """Valid code should produce no diagnostics."""
    await open_doc(client, "(define x 42)")
    # pytest-lsp captures diagnostics automatically
    assert len(client.diagnostics) == 0 or all(
        len(diags) == 0 for diags in client.diagnostics.values()
    )


@pytest.mark.asyncio
async def test_parse_error_produces_diagnostic(client: LanguageClient):
    """Unmatched paren should produce a parse error diagnostic."""
    uri = await open_doc(client, "(define x", name="broken.sema")
    diags = client.diagnostics.get(uri, [])
    assert len(diags) >= 1
    assert diags[0].severity == DiagnosticSeverity.Error
