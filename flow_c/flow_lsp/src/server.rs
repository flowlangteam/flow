use crate::analyzer_bridge::AnalyzerBridge;
use crate::document::DocumentManager;
use crate::handlers::Handlers;
use crate::utils::is_flow_file;
use std::sync::Arc;
use tokio::sync::RwLock;
use tower_lsp::lsp_types::{
    CompletionOptions, CompletionParams, CompletionResponse, DidChangeTextDocumentParams,
    DidCloseTextDocumentParams, DidOpenTextDocumentParams, DocumentSymbolParams,
    DocumentSymbolResponse, GotoDefinitionParams, GotoDefinitionResponse, Hover, HoverParams,
    HoverProviderCapability, InitializeParams, InitializeResult, InitializedParams, MessageType,
    ReferenceParams, ServerCapabilities, ServerInfo, TextDocumentSyncCapability,
    TextDocumentSyncKind,
};
use tower_lsp::{Client, LanguageServer, LspService, Server, jsonrpc::Result};

pub struct FlowLanguageServer {
    client: Client,
    document_manager: Arc<DocumentManager>,
    analyzer_bridge: Arc<RwLock<AnalyzerBridge>>,
    handlers: Handlers,
}

impl FlowLanguageServer {
    pub fn new(client: Client) -> Self {
        let document_manager = Arc::new(DocumentManager::new());
        let analyzer_bridge = Arc::new(RwLock::new(AnalyzerBridge::new()));
        let handlers = Handlers::new(document_manager.clone(), analyzer_bridge.clone());

        Self {
            client,
            document_manager,
            analyzer_bridge,
            handlers,
        }
    }

    /// Create and run the language server
    pub async fn run() {
        let stdin = tokio::io::stdin();
        let stdout = tokio::io::stdout();

        let (service, socket) = LspService::new(|client| FlowLanguageServer::new(client));

        tracing::info!("Starting Flow Language Server");
        Server::new(stdin, stdout, socket).serve(service).await;
    }

    /// Publish diagnostics for a document
    async fn publish_diagnostics_for_document(&self, uri: tower_lsp::lsp_types::Url) {
        if let Some(diagnostics) = self.handlers.publish_diagnostics(&uri).await {
            self.client
                .publish_diagnostics(uri, diagnostics, None)
                .await;
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for FlowLanguageServer {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        tracing::info!("Initializing Flow Language Server");

        // Log client info
        if let Some(client_info) = params.client_info {
            tracing::info!(
                "Client: {} {}",
                client_info.name,
                client_info.version.unwrap_or_default()
            );
        }

        let server_capabilities = ServerCapabilities {
            // Text document synchronization
            text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),

            // Hover support
            hover_provider: Some(HoverProviderCapability::Simple(true)),

            // Completion support
            completion_provider: Some(CompletionOptions {
                resolve_provider: Some(false),
                trigger_characters: Some(vec![".".to_string(), ":".to_string()]),
                work_done_progress_options: Default::default(),
                all_commit_characters: None,
                completion_item: None,
            }),

            // Go to definition
            definition_provider: Some(tower_lsp::lsp_types::OneOf::Left(true)),

            // Find references
            references_provider: Some(tower_lsp::lsp_types::OneOf::Left(true)),

            // Document symbols
            document_symbol_provider: Some(tower_lsp::lsp_types::OneOf::Left(true)),

            // Other capabilities can be added here
            ..Default::default()
        };

        Ok(InitializeResult {
            capabilities: server_capabilities,
            server_info: Some(ServerInfo {
                name: "Flow Language Server".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        tracing::info!("Flow Language Server initialized");
        self.client
            .log_message(MessageType::INFO, "Flow Language Server started")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        tracing::info!("Shutting down Flow Language Server");
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri.clone();

        tracing::info!("Document opened: {}", uri);

        if is_flow_file(&uri) {
            // Add document to manager
            self.document_manager.open_document(
                uri.clone(),
                params.text_document.text,
                params.text_document.version,
                params.text_document.language_id,
            );

            // Publish initial diagnostics
            self.publish_diagnostics_for_document(uri).await;
        }
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.clone();

        tracing::debug!("Document changed: {}", uri);

        if is_flow_file(&uri) {
            // Update document
            if let Err(e) = self.document_manager.change_document(
                &uri,
                params.text_document.version,
                params.content_changes,
            ) {
                tracing::error!("Failed to update document {}: {}", uri, e);
                return;
            }

            // Re-analyze and publish diagnostics
            self.publish_diagnostics_for_document(uri).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;

        tracing::info!("Document closed: {}", uri);

        if is_flow_file(&uri) {
            // Remove document from manager
            self.document_manager.close_document(&uri);

            // Clear diagnostics
            self.client.publish_diagnostics(uri, vec![], None).await;
        }
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        tracing::debug!(
            "Completion request at {}:{}",
            params.text_document_position.position.line,
            params.text_document_position.position.character
        );

        Ok(self.handlers.completion(params).await)
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        tracing::debug!(
            "Hover request at {}:{}",
            params.text_document_position_params.position.line,
            params.text_document_position_params.position.character
        );

        Ok(self.handlers.hover(params).await)
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        tracing::debug!(
            "Go to definition request at {}:{}",
            params.text_document_position_params.position.line,
            params.text_document_position_params.position.character
        );

        Ok(self.handlers.goto_definition(params).await)
    }

    async fn references(
        &self,
        params: ReferenceParams,
    ) -> Result<Option<Vec<tower_lsp::lsp_types::Location>>> {
        tracing::debug!(
            "Find references request at {}:{}",
            params.text_document_position.position.line,
            params.text_document_position.position.character
        );

        Ok(self.handlers.find_references(params).await)
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        tracing::debug!("Document symbol request for {}", params.text_document.uri);

        Ok(self.handlers.document_symbols(params).await)
    }
}
