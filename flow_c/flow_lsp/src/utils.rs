use tower_lsp::lsp_types::{Position, Range, Url};
use std::path::Path;

/// Convert a file path to a URI
pub fn path_to_uri(path: &Path) -> Url {
    Url::from_file_path(path).unwrap_or_else(|_| {
        // Fallback for invalid paths
        Url::parse(&format!("file://{}", path.display())).unwrap()
    })
}

/// Convert a URI to a file path
pub fn uri_to_path(uri: &Url) -> Option<std::path::PathBuf> {
    uri.to_file_path().ok()
}

/// Check if a URI represents a Flow source file
pub fn is_flow_file(uri: &Url) -> bool {
    uri.path().ends_with(".flow")
}

/// Create an LSP Range from start and end positions
pub fn create_range(start_line: u32, start_char: u32, end_line: u32, end_char: u32) -> Range {
    Range {
        start: Position {
            line: start_line,
            character: start_char,
        },
        end: Position {
            line: end_line,
            character: end_char,
        },
    }
}

/// Check if a position is within a range
pub fn position_in_range(position: &Position, range: &Range) -> bool {
    if position.line < range.start.line || position.line > range.end.line {
        return false;
    }
    
    if position.line == range.start.line && position.character < range.start.character {
        return false;
    }
    
    if position.line == range.end.line && position.character > range.end.character {
        return false;
    }
    
    true
}

/// Get the line and column from an offset in text
pub fn offset_to_line_col(text: &str, offset: usize) -> (u32, u32) {
    let mut line = 0;
    let mut col = 0;
    
    for (i, ch) in text.char_indices() {
        if i >= offset {
            break;
        }
        
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }
    
    (line, col)
}

/// Get offset from line and column
pub fn line_col_to_offset(text: &str, target_line: u32, target_col: u32) -> Option<usize> {
    let mut line = 0;
    let mut col = 0;
    
    for (i, ch) in text.char_indices() {
        if line == target_line && col == target_col {
            return Some(i);
        }
        
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }
    
    // Handle end of file
    if line == target_line && col == target_col {
        Some(text.len())
    } else {
        None
    }
}

/// Normalize line endings to LF
pub fn normalize_line_endings(text: &str) -> String {
    text.replace("\r\n", "\n").replace('\r', "\n")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_offset_to_line_col() {
        let text = "hello\nworld\ntest";
        assert_eq!(offset_to_line_col(text, 0), (0, 0));
        assert_eq!(offset_to_line_col(text, 5), (0, 5));
        assert_eq!(offset_to_line_col(text, 6), (1, 0));
        assert_eq!(offset_to_line_col(text, 12), (2, 0));
    }

    #[test]
    fn test_line_col_to_offset() {
        let text = "hello\nworld\ntest";
        assert_eq!(line_col_to_offset(text, 0, 0), Some(0));
        assert_eq!(line_col_to_offset(text, 0, 5), Some(5));
        assert_eq!(line_col_to_offset(text, 1, 0), Some(6));
        assert_eq!(line_col_to_offset(text, 2, 0), Some(12));
    }

    #[test]
    fn test_position_in_range() {
        let range = create_range(1, 5, 3, 10);
        
        // Position before range
        assert!(!position_in_range(&Position { line: 0, character: 0 }, &range));
        
        // Position at start of range
        assert!(position_in_range(&Position { line: 1, character: 5 }, &range));
        
        // Position inside range
        assert!(position_in_range(&Position { line: 2, character: 0 }, &range));
        
        // Position at end of range
        assert!(position_in_range(&Position { line: 3, character: 10 }, &range));
        
        // Position after range
        assert!(!position_in_range(&Position { line: 4, character: 0 }, &range));
    }
}