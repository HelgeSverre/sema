"""Shared helpers for LSP e2e tests."""

import pathlib
import tempfile
import textwrap

from lsprotocol.types import (
    DidOpenTextDocumentParams,
    TextDocumentIdentifier,
    TextDocumentItem,
)
from pytest_lsp import LanguageClient


async def open_doc(client: LanguageClient, text: str, name: str = "test.sema") -> str:
    """Write text to a temp file and open it in the LSP client. Returns the URI."""
    tmp = pathlib.Path(tempfile.mkdtemp(prefix="sema-lsp-")) / name
    tmp.write_text(textwrap.dedent(text))
    uri = tmp.as_uri()

    client.text_document_did_open(
        DidOpenTextDocumentParams(
            text_document=TextDocumentItem(
                uri=uri,
                language_id="sema",
                version=1,
                text=textwrap.dedent(text),
            )
        )
    )
    # Give the server time to parse
    import asyncio
    await asyncio.sleep(0.3)
    return uri
