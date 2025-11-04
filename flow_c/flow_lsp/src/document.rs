use anyhow::Result;
use dashmap::DashMap;
use tower_lsp::lsp_types::{Position, Range, TextDocumentContentChangeEvent, Url};
use ropey::Rope;

/// Represents a document tracked by the LSP server
#[derive(Debug, Clone)]
pub struct Document {
    pub uri: Url,
    pub text: Rope,
    pub version: i32,
    pub language_id: String,
}

impl Document {
    pub fn new(uri: Url, text: String, version: i32, language_id: String) -> Self {
        Self {
            uri,
            text: Rope::from_str(&text),
            version,
            language_id,
        }
    }

    /// Apply a text change to the document
    pub fn apply_change(&mut self, change: &TextDocumentContentChangeEvent) -> Result<()> {
        match &change.range {
            Some(range) => {
                // Incremental change
                let start_offset = self.position_to_offset(&range.start)?;
                let end_offset = self.position_to_offset(&range.end)?;
                
                // Replace the text in the range
                self.text.remove(start_offset..end_offset);
                self.text.insert(start_offset, &change.text);
            }
            None => {
                // Full document replacement
                self.text = Rope::from_str(&change.text);
            }
        }
        Ok(())
    }

    /// Get text for a specific range
    pub fn get_text_range(&self, range: &Range) -> Result<String> {
        let start_offset = self.position_to_offset(&range.start)?;
        let end_offset = self.position_to_offset(&range.end)?;
        Ok(self.text.slice(start_offset..end_offset).to_string())
    }

    /// Get the entire document text as a string
    pub fn get_text(&self) -> String {
        self.text.to_string()
    }

    /// Convert LSP Position to byte offset
    pub fn position_to_offset(&self, position: &Position) -> Result<usize> {
        let line_idx = position.line as usize;
        let char_idx = position.character as usize;
        
        if line_idx >= self.text.len_lines() {
            return Ok(self.text.len_chars());
        }
        
        let line_start = self.text.line_to_char(line_idx);
        let line = self.text.line(line_idx);
        let line_len = line.len_chars().saturating_sub(1); // Subtract newline
        
        let char_offset = char_idx.min(line_len);
        Ok(line_start + char_offset)
    }

    /// Convert byte offset to LSP Position
    pub fn offset_to_position(&self, target_offset: usize) -> Position {
        let char_idx = target_offset.min(self.text.len_chars());
        let line_idx = self.text.char_to_line(char_idx);
        let line_start = self.text.line_to_char(line_idx);
        let character = char_idx - line_start;
        
        Position {
            line: line_idx as u32,
            character: character as u32,
        }
    }

    /// Get the word at the given position
    pub fn word_at_position(&self, position: &Position) -> Option<String> {
        let offset = self.position_to_offset(position).ok()?;
        let text = self.get_text();
        let chars: Vec<char> = text.chars().collect();
        
        if offset >= chars.len() {
            return None;
        }
        
        // Find word boundaries
        let mut start = offset;
        let mut end = offset;
        
        // Move start backwards to find word start
        while start > 0 && (chars[start - 1].is_alphanumeric() || chars[start - 1] == '_') {
            start -= 1;
        }
        
        // Move end forwards to find word end
        while end < chars.len() && (chars[end].is_alphanumeric() || chars[end] == '_') {
            end += 1;
        }
        
        if start == end {
            return None;
        }
        
        Some(chars[start..end].iter().collect())
    }
}

/// Manages all documents in the LSP server
#[derive(Debug)]
pub struct DocumentManager {
    documents: DashMap<Url, Document>,
}

impl DocumentManager {
    pub fn new() -> Self {
        Self {
            documents: DashMap::new(),
        }
    }

    /// Open a new document
    pub fn open_document(&self, uri: Url, text: String, version: i32, language_id: String) {
        let document = Document::new(uri.clone(), text, version, language_id);
        self.documents.insert(uri, document);
    }

    /// Close a document
    pub fn close_document(&self, uri: &Url) {
        self.documents.remove(uri);
    }

    /// Apply changes to a document
    pub fn change_document(
        &self,
        uri: &Url,
        version: i32,
        changes: Vec<TextDocumentContentChangeEvent>,
    ) -> Result<()> {
        if let Some(mut document) = self.documents.get_mut(uri) {
            document.version = version;
            for change in changes {
                document.apply_change(&change)?;
            }
        }
        Ok(())
    }

    /// Get a document by URI
    pub fn get_document(&self, uri: &Url) -> Option<Document> {
        self.documents.get(uri).map(|doc| doc.clone())
    }

    /// Get all document URIs
    pub fn get_all_uris(&self) -> Vec<Url> {
        self.documents.iter().map(|entry| entry.key().clone()).collect()
    }
}

impl Default for DocumentManager {
    fn default() -> Self {
        Self::new()
    }
}