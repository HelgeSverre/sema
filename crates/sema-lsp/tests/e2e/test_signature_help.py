"""Test textDocument/signatureHelp."""

import pytest
from lsprotocol.types import (
    Position,
    SignatureHelpParams,
    TextDocumentIdentifier,
)
from pytest_lsp import LanguageClient

from helpers import open_doc


@pytest.mark.asyncio
async def test_signature_help_user_function(client: LanguageClient):
    """Signature help should show params for user-defined functions."""
    uri = await open_doc(client, "(defun add (a b) (+ a b))\n(add )")
    result = await client.text_document_signature_help_async(
        SignatureHelpParams(
            text_document=TextDocumentIdentifier(uri=uri),
            position=Position(line=1, character=5),  # inside (add |)
        )
    )
    assert result is not None
    assert len(result.signatures) >= 1
    sig = result.signatures[0]
    assert "add" in sig.label
    assert sig.parameters is not None
    assert len(sig.parameters) == 2


@pytest.mark.asyncio
async def test_signature_help_active_parameter(client: LanguageClient):
    """Active parameter should track cursor position."""
    uri = await open_doc(client, "(defun f (x y z) x)\n(f 1 2 )")
    result = await client.text_document_signature_help_async(
        SignatureHelpParams(
            text_document=TextDocumentIdentifier(uri=uri),
            position=Position(line=1, character=7),  # after second arg
        )
    )
    assert result is not None
    assert result.active_parameter is not None
    assert result.active_parameter >= 2  # on third parameter


@pytest.mark.asyncio
async def test_signature_help_no_function(client: LanguageClient):
    """Signature help outside a call should return None."""
    uri = await open_doc(client, "(define x 42)")
    result = await client.text_document_signature_help_async(
        SignatureHelpParams(
            text_document=TextDocumentIdentifier(uri=uri),
            position=Position(line=0, character=13),
        )
    )
    assert result is None


@pytest.mark.asyncio
async def test_signature_help_builtin(client: LanguageClient):
    """Signature help for builtins should show documentation."""
    uri = await open_doc(client, "(map )")
    result = await client.text_document_signature_help_async(
        SignatureHelpParams(
            text_document=TextDocumentIdentifier(uri=uri),
            position=Position(line=0, character=5),
        )
    )
    assert result is not None
    assert len(result.signatures) >= 1
