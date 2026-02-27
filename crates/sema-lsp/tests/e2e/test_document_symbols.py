"""Test textDocument/documentSymbol."""

import pytest
from lsprotocol.types import (
    DocumentSymbolParams,
    TextDocumentIdentifier,
    SymbolKind,
)
from pytest_lsp import LanguageClient

from helpers import open_doc


@pytest.mark.asyncio
async def test_document_symbols_defun(client: LanguageClient):
    """Document symbols should include defun definitions."""
    uri = await open_doc(client, "(defun add (a b) (+ a b))")
    result = await client.text_document_document_symbol_async(
        DocumentSymbolParams(
            text_document=TextDocumentIdentifier(uri=uri),
        )
    )
    assert result is not None
    assert len(result) == 1
    assert result[0].name == "add"
    assert result[0].kind == SymbolKind.Function


@pytest.mark.asyncio
async def test_document_symbols_define_variable(client: LanguageClient):
    """Document symbols should include define variable bindings."""
    uri = await open_doc(client, "(define pi 3.14)")
    result = await client.text_document_document_symbol_async(
        DocumentSymbolParams(
            text_document=TextDocumentIdentifier(uri=uri),
        )
    )
    assert result is not None
    assert len(result) == 1
    assert result[0].name == "pi"
    assert result[0].kind == SymbolKind.Variable


@pytest.mark.asyncio
async def test_document_symbols_multiple(client: LanguageClient):
    """Multiple definitions should produce multiple symbols."""
    uri = await open_doc(
        client,
        "(define x 1)\n(defun f (a) a)\n(defmacro m (x) x)",
    )
    result = await client.text_document_document_symbol_async(
        DocumentSymbolParams(
            text_document=TextDocumentIdentifier(uri=uri),
        )
    )
    assert result is not None
    assert len(result) == 3
    names = [s.name for s in result]
    assert "x" in names
    assert "f" in names
    assert "m" in names


@pytest.mark.asyncio
async def test_document_symbols_empty_doc(client: LanguageClient):
    """Empty document should produce no symbols."""
    uri = await open_doc(client, "")
    result = await client.text_document_document_symbol_async(
        DocumentSymbolParams(
            text_document=TextDocumentIdentifier(uri=uri),
        )
    )
    assert result is None or len(result) == 0


@pytest.mark.asyncio
async def test_document_symbols_no_definitions(client: LanguageClient):
    """Code without definitions should produce no symbols."""
    uri = await open_doc(client, "(+ 1 2)\n(println \"hello\")")
    result = await client.text_document_document_symbol_async(
        DocumentSymbolParams(
            text_document=TextDocumentIdentifier(uri=uri),
        )
    )
    assert result is None or len(result) == 0


@pytest.mark.asyncio
async def test_document_symbols_function_has_detail(client: LanguageClient):
    """Function symbols should have a detail with parameter info."""
    uri = await open_doc(client, "(defun greet (name) (println name))")
    result = await client.text_document_document_symbol_async(
        DocumentSymbolParams(
            text_document=TextDocumentIdentifier(uri=uri),
        )
    )
    assert result is not None
    assert len(result) == 1
    assert result[0].detail is not None
    assert "name" in result[0].detail
