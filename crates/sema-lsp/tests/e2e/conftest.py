"""pytest-lsp end-to-end tests for the Sema language server."""

import asyncio
import os
import pathlib
import tempfile

import pytest
import pytest_lsp
from lsprotocol.types import (
    ClientCapabilities,
    GeneralClientCapabilities,
    InitializeParams,
    TextDocumentClientCapabilities,
    CompletionClientCapabilities,
    HoverClientCapabilities,
    RenameClientCapabilities,
    SignatureHelpClientCapabilities,
    WorkspaceClientCapabilities,
)
from pytest_lsp import ClientServerConfig, LanguageClient

SEMA_BIN = os.environ.get(
    "SEMA_BIN",
    str(pathlib.Path(__file__).resolve().parents[4] / "target" / "release" / "sema"),
)

_tmpdir = tempfile.mkdtemp(prefix="sema-lsp-test-")
WORKSPACE_URI = pathlib.Path(_tmpdir).as_uri()


@pytest_lsp.fixture(
    config=ClientServerConfig(
        server_command=[SEMA_BIN, "lsp"],
        server_env={"RUST_LOG": "warn"},
    ),
)
async def client(lsp_client: LanguageClient):
    params = InitializeParams(
        capabilities=ClientCapabilities(
            general=GeneralClientCapabilities(),
            text_document=TextDocumentClientCapabilities(
                completion=CompletionClientCapabilities(),
                hover=HoverClientCapabilities(),
                rename=RenameClientCapabilities(prepare_support=True),
                signature_help=SignatureHelpClientCapabilities(),
            ),
            workspace=WorkspaceClientCapabilities(),
        ),
        root_uri=WORKSPACE_URI,
    )
    response = await lsp_client.initialize_session(params)
    lsp_client.server_capabilities = response.capabilities
    yield

    # Custom shutdown with timeout — works around tower-lsp#399 where
    # the server process may not exit after the exit notification.
    if lsp_client.capabilities is not None:
        await lsp_client.shutdown_async(None)
        lsp_client.exit(None)
    if lsp_client._server:
        try:
            await asyncio.wait_for(lsp_client._server.wait(), timeout=5.0)
        except asyncio.TimeoutError:
            lsp_client._server.kill()
            await lsp_client._server.wait()
