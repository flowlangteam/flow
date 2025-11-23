use crate::analyzer_bridge::AnalyzerBridge;
use crate::document::{Document, DocumentManager};
use crate::utils::is_flow_file;
use std::sync::Arc;
use tokio::sync::RwLock;
use tower_lsp::lsp_types::{
    CompletionParams, CompletionResponse, Diagnostic, GotoDefinitionParams, GotoDefinitionResponse,
    Hover, HoverContents, HoverParams, Location, MarkedString, Position, Range, Url,
};

pub struct Handlers {
    document_manager: Arc<DocumentManager>,
    analyzer_bridge: Arc<RwLock<AnalyzerBridge>>,
}

impl Handlers {
    pub fn new(
        document_manager: Arc<DocumentManager>,
        analyzer_bridge: Arc<RwLock<AnalyzerBridge>>,
    ) -> Self {
        Self {
            document_manager,
            analyzer_bridge,
        }
    }

    /// Handle textDocument/publishDiagnostics
    pub async fn publish_diagnostics(&self, uri: &Url) -> Option<Vec<Diagnostic>> {
        if !is_flow_file(uri) {
            return None;
        }

        let document = self.document_manager.get_document(uri)?;
        let text = document.get_text();

        let mut analyzer = self.analyzer_bridge.write().await;
        Some(analyzer.analyze_document(uri, &text))
    }

    /// Handle textDocument/completion
    pub async fn completion(&self, params: CompletionParams) -> Option<CompletionResponse> {
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        if !is_flow_file(uri) {
            return None;
        }

        let document = self.document_manager.get_document(uri)?;
        let text = document.get_text();

        let mut analyzer = self.analyzer_bridge.write().await;
        let items = analyzer.get_completions(&text, position);

        Some(CompletionResponse::Array(items))
    }

    /// Handle textDocument/hover
    pub async fn hover(&self, params: HoverParams) -> Option<Hover> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        if !is_flow_file(uri) {
            return None;
        }

        let document = self.document_manager.get_document(uri)?;
        let text = document.get_text();

        // Get the word at the position
        let word = document.word_at_position(&position)?;

        let analyzer = self.analyzer_bridge.write().await;
        let hover_info = analyzer.get_hover_info(&text, position, &word)?;

        // Create a simple hover response with code formatting
        let contents = HoverContents::Scalar(MarkedString::LanguageString(
            tower_lsp::lsp_types::LanguageString {
                language: "flow".to_string(),
                value: hover_info,
            },
        ));

        Some(Hover {
            contents,
            range: Some(self.get_word_range(&document, &position, &word)),
        })
    }

    /// Handle textDocument/definition
    pub async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Option<GotoDefinitionResponse> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        if !is_flow_file(uri) {
            return None;
        }

        let document = self.document_manager.get_document(uri)?;
        let word = document.word_at_position(&position)?;

        // @TODO: Implement proper AST-based go-to-definition with full semantic analysis
        // For now, return a simple implementation that finds function definitions
        // In a real implementation, you'd traverse the AST to find the actual definition
        let definition_location = self.find_definition(&document, &word)?;

        Some(GotoDefinitionResponse::Scalar(definition_location))
    }

    /// Handle textDocument/references
    pub async fn find_references(
        &self,
        params: tower_lsp::lsp_types::ReferenceParams,
    ) -> Option<Vec<Location>> {
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        if !is_flow_file(uri) {
            return None;
        }

        let document = self.document_manager.get_document(uri)?;
        let word = document.word_at_position(&position)?;

        // Simple implementation that finds all occurrences of the word
        let references = self.find_word_references(&document, &word);

        if references.is_empty() {
            None
        } else {
            Some(references)
        }
    }

    /// Handle textDocument/documentSymbol
    pub async fn document_symbols(
        &self,
        params: tower_lsp::lsp_types::DocumentSymbolParams,
    ) -> Option<tower_lsp::lsp_types::DocumentSymbolResponse> {
        let uri = &params.text_document.uri;

        if !is_flow_file(uri) {
            return None;
        }

        let document = self.document_manager.get_document(uri)?;
        let text = document.get_text();

        // Parse and extract symbols
        let symbols = self.extract_document_symbols(&text, uri)?;

        Some(tower_lsp::lsp_types::DocumentSymbolResponse::Flat(symbols))
    }

    /// Helper method to get the range of a word at a position
    fn get_word_range(&self, document: &Document, position: &Position, _word: &str) -> Range {
        // Simple implementation - find the word boundaries
        let offset = document.position_to_offset(position).unwrap_or(0);
        let text = document.get_text();

        // Find start of word
        let mut start_offset = offset;
        let chars: Vec<char> = text.chars().collect();

        while start_offset > 0
            && (chars[start_offset - 1].is_alphanumeric() || chars[start_offset - 1] == '_')
        {
            start_offset -= 1;
        }

        // Find end of word
        let mut end_offset = offset;
        while end_offset < chars.len()
            && (chars[end_offset].is_alphanumeric() || chars[end_offset] == '_')
        {
            end_offset += 1;
        }

        let start_pos = document.offset_to_position(start_offset);
        let end_pos = document.offset_to_position(end_offset);

        Range {
            start: start_pos,
            end: end_pos,
        }
    }

    /// Simple definition finder (to be improved with proper AST traversal)
    fn find_definition(&self, document: &Document, word: &str) -> Option<Location> {
        let text = document.get_text();
        let lines: Vec<&str> = text.lines().collect();

        // Look for function definitions
        for (line_idx, line) in lines.iter().enumerate() {
            if line.contains("func") && line.contains(word) {
                // Found a potential function definition
                if let Some(func_start) = line.find("func") {
                    if let Some(name_start) = line[func_start..].find(word) {
                        let char_pos = func_start + name_start;
                        return Some(Location {
                            uri: document.uri.clone(),
                            range: Range {
                                start: Position {
                                    line: line_idx as u32,
                                    character: char_pos as u32,
                                },
                                end: Position {
                                    line: line_idx as u32,
                                    character: (char_pos + word.len()) as u32,
                                },
                            },
                        });
                    }
                }
            }
        }

        None
    }

    /// Find all references to a word in the document
    fn find_word_references(&self, document: &Document, word: &str) -> Vec<Location> {
        let text = document.get_text();
        let lines: Vec<&str> = text.lines().collect();
        let mut references = Vec::new();

        for (line_idx, line) in lines.iter().enumerate() {
            let mut start = 0;
            while let Some(pos) = line[start..].find(word) {
                let actual_pos = start + pos;

                // Check if it's a whole word (not part of another identifier)
                let is_word_boundary = {
                    let before_ok = actual_pos == 0
                        || !line
                            .chars()
                            .nth(actual_pos - 1)
                            .unwrap_or(' ')
                            .is_alphanumeric();
                    let after_ok = actual_pos + word.len() >= line.len()
                        || !line
                            .chars()
                            .nth(actual_pos + word.len())
                            .unwrap_or(' ')
                            .is_alphanumeric();
                    before_ok && after_ok
                };

                if is_word_boundary {
                    references.push(Location {
                        uri: document.uri.clone(),
                        range: Range {
                            start: Position {
                                line: line_idx as u32,
                                character: actual_pos as u32,
                            },
                            end: Position {
                                line: line_idx as u32,
                                character: (actual_pos + word.len()) as u32,
                            },
                        },
                    });
                }

                start = actual_pos + 1;
            }
        }

        references
    }

    /// Extract document symbols (functions, structs, etc.)
    fn extract_document_symbols(
        &self,
        text: &str,
        uri: &Url,
    ) -> Option<Vec<tower_lsp::lsp_types::SymbolInformation>> {
        use flow_ast::Item;
        use flow_parser::Parser;
        use tower_lsp::lsp_types::{SymbolInformation, SymbolKind};

        let mut parser = Parser::new(text);
        let program = parser.parse().ok()?;
        let mut symbols = Vec::new();

        for item in &program.items {
            match item {
                Item::Function(func) => {
                    symbols.push(SymbolInformation {
                        name: func.name.clone(),
                        kind: SymbolKind::FUNCTION,
                        tags: None,
                        #[allow(deprecated)]
                        deprecated: None,
                        location: Location {
                            uri: uri.clone(),
                            range: Range {
                                start: Position {
                                    line: func.span.start as u32, // @TODO: Implement proper span-to-position conversion
                                    character: 0,
                                },
                                end: Position {
                                    line: func.span.end as u32, // @TODO: Implement proper span-to-position conversion
                                    character: 0,
                                },
                            },
                        },
                        container_name: None,
                    });
                }
                Item::Struct(struct_def) => {
                    symbols.push(SymbolInformation {
                        name: struct_def.name.clone(),
                        kind: SymbolKind::STRUCT,
                        tags: None,
                        #[allow(deprecated)]
                        deprecated: None,
                        location: Location {
                            uri: uri.clone(),
                            range: Range {
                                start: Position {
                                    line: 0,
                                    character: 0,
                                }, // Simplified
                                end: Position {
                                    line: 0,
                                    character: 0,
                                },
                            },
                        },
                        container_name: None,
                    });
                }
                _ => {}
            }
        }

        Some(symbols)
    }
}
