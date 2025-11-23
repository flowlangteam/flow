use crate::Span;
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DiagnosticSeverity {
    Error,
    Warning,
    Note,
    Help,
}

impl fmt::Display for DiagnosticSeverity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DiagnosticSeverity::Error => write!(f, "error"),
            DiagnosticSeverity::Warning => write!(f, "warning"),
            DiagnosticSeverity::Note => write!(f, "note"),
            DiagnosticSeverity::Help => write!(f, "help"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LabelStyle {
    Primary,
    Secondary,
    Help,
    Note,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Label {
    pub span: Span,
    pub message: Option<String>,
    pub style: LabelStyle,
}

impl Label {
    pub fn new(span: Span, style: LabelStyle) -> Self {
        Self {
            span,
            message: None,
            style,
        }
    }

    pub fn primary(span: Span) -> Self {
        Self::new(span, LabelStyle::Primary)
    }

    pub fn secondary(span: Span) -> Self {
        Self::new(span, LabelStyle::Secondary)
    }

    pub fn with_message(mut self, message: impl Into<String>) -> Self {
        self.message = Some(message.into());
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Replacement {
    pub span: Span,
    pub text: String,
}

impl Replacement {
    pub fn new(span: Span, text: impl Into<String>) -> Self {
        Self {
            span,
            text: text.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Suggestion {
    pub message: String,
    pub replacements: Vec<Replacement>,
}

impl Suggestion {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            replacements: Vec::new(),
        }
    }

    pub fn with_replacement(mut self, replacement: Replacement) -> Self {
        self.replacements.push(replacement);
        self
    }

    pub fn with_replacements(mut self, replacements: Vec<Replacement>) -> Self {
        self.replacements.extend(replacements);
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostic {
    pub code: Option<String>,
    pub message: String,
    pub severity: DiagnosticSeverity,
    pub labels: Vec<Label>,
    pub suggestions: Vec<Suggestion>,
    pub notes: Vec<String>,
}

impl Diagnostic {
    pub fn new(severity: DiagnosticSeverity, message: impl Into<String>) -> Self {
        Self {
            code: None,
            message: message.into(),
            severity,
            labels: Vec::new(),
            suggestions: Vec::new(),
            notes: Vec::new(),
        }
    }

    pub fn error(message: impl Into<String>) -> Self {
        Self::new(DiagnosticSeverity::Error, message)
    }

    pub fn warning(message: impl Into<String>) -> Self {
        Self::new(DiagnosticSeverity::Warning, message)
    }

    pub fn note(message: impl Into<String>) -> Self {
        Self::new(DiagnosticSeverity::Note, message)
    }

    pub fn with_code(mut self, code: impl Into<String>) -> Self {
        self.code = Some(code.into());
        self
    }

    pub fn with_label(mut self, label: Label) -> Self {
        self.labels.push(label);
        self
    }

    pub fn with_labels(mut self, labels: Vec<Label>) -> Self {
        self.labels.extend(labels);
        self
    }

    pub fn with_suggestion(mut self, suggestion: Suggestion) -> Self {
        self.suggestions.push(suggestion);
        self
    }

    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }

    pub fn with_notes(mut self, notes: Vec<String>) -> Self {
        self.notes.extend(notes);
        self
    }
}

/// Collector for diagnostics during compilation phases
#[derive(Debug, Clone, Default)]
pub struct DiagnosticCollector {
    diagnostics: Vec<Diagnostic>,
    error_count: usize,
    warning_count: usize,
}

impl DiagnosticCollector {
    pub fn new() -> Self {
        Self {
            diagnostics: Vec::new(),
            error_count: 0,
            warning_count: 0,
        }
    }

    pub fn add(&mut self, diagnostic: Diagnostic) {
        match diagnostic.severity {
            DiagnosticSeverity::Error => self.error_count += 1,
            DiagnosticSeverity::Warning => self.warning_count += 1,
            _ => {}
        }
        self.diagnostics.push(diagnostic);
    }

    pub fn add_error(&mut self, message: impl Into<String>, span: Span) {
        self.add(Diagnostic::error(message).with_label(Label::primary(span)));
    }

    pub fn add_warning(&mut self, message: impl Into<String>, span: Span) {
        self.add(Diagnostic::warning(message).with_label(Label::primary(span)));
    }

    pub fn extend(&mut self, other: DiagnosticCollector) {
        self.error_count += other.error_count;
        self.warning_count += other.warning_count;
        self.diagnostics.extend(other.diagnostics);
    }

    pub fn has_errors(&self) -> bool {
        self.error_count > 0
    }

    pub fn has_warnings(&self) -> bool {
        self.warning_count > 0
    }

    pub fn error_count(&self) -> usize {
        self.error_count
    }

    pub fn warning_count(&self) -> usize {
        self.warning_count
    }

    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    pub fn into_diagnostics(self) -> Vec<Diagnostic> {
        self.diagnostics
    }

    pub fn clear(&mut self) {
        self.diagnostics.clear();
        self.error_count = 0;
        self.warning_count = 0;
    }
}

/// Common diagnostic error codes
pub mod error_codes {
    pub const E0001: &str = "E0001"; // Syntax error
    pub const E0002: &str = "E0002"; // Type mismatch
    pub const E0003: &str = "E0003"; // Undefined variable
    pub const E0004: &str = "E0004"; // Undefined function
    pub const E0005: &str = "E0005"; // Undefined type
    pub const E0006: &str = "E0006"; // Invalid number of arguments
    pub const E0007: &str = "E0007"; // Invalid type argument
    pub const E0008: &str = "E0008"; // Duplicate definition
    pub const E0009: &str = "E0009"; // Invalid extern block
    pub const E0010: &str = "E0010"; // Unsupported feature
    pub const E0011: &str = "E0011"; // Invalid field access
    pub const E0012: &str = "E0012"; // Missing field
    pub const E0013: &str = "E0013"; // Immutable assignment
    pub const E0014: &str = "E0014"; // Invalid return type
    pub const E0015: &str = "E0015"; // Unreachable code
    pub const E0016: &str = "E0016"; // Pattern match not exhaustive
    pub const E0017: &str = "E0017"; // Invalid pattern
    pub const E0018: &str = "E0018"; // Unsafe operation outside unsafe block
    pub const E0019: &str = "E0019"; // Memory leak detected
    pub const E0020: &str = "E0020"; // Invalid pointer operation
}

/// Common diagnostic warning codes
pub mod warning_codes {
    pub const W0001: &str = "W0001"; // Unused variable
    pub const W0002: &str = "W0002"; // Unused function
    pub const W0003: &str = "W0003"; // Dead code
    pub const W0004: &str = "W0004"; // Unnecessary mutable
    pub const W0005: &str = "W0005"; // Implicit conversion
    pub const W0006: &str = "W0006"; // Shadowed variable
    pub const W0007: &str = "W0007"; // Deprecated feature
    pub const W0008: &str = "W0008"; // Inefficient operation
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_diagnostic_creation() {
        let diag = Diagnostic::error("Test error")
            .with_code("E0001")
            .with_label(Label::primary(Span::new(0, 10)).with_message("here"))
            .with_note("This is a note");

        assert_eq!(diag.severity, DiagnosticSeverity::Error);
        assert_eq!(diag.code, Some("E0001".to_string()));
        assert_eq!(diag.labels.len(), 1);
        assert_eq!(diag.notes.len(), 1);
    }

    #[test]
    fn test_collector() {
        let mut collector = DiagnosticCollector::new();

        collector.add_error("Error 1", Span::new(0, 5));
        collector.add_warning("Warning 1", Span::new(10, 15));
        collector.add_error("Error 2", Span::new(20, 25));

        assert_eq!(collector.error_count(), 2);
        assert_eq!(collector.warning_count(), 1);
        assert!(collector.has_errors());
        assert!(collector.has_warnings());
    }

    #[test]
    fn test_collector_extend() {
        let mut collector1 = DiagnosticCollector::new();
        collector1.add_error("Error 1", Span::new(0, 5));

        let mut collector2 = DiagnosticCollector::new();
        collector2.add_warning("Warning 1", Span::new(10, 15));

        collector1.extend(collector2);

        assert_eq!(collector1.error_count(), 1);
        assert_eq!(collector1.warning_count(), 1);
        assert_eq!(collector1.diagnostics().len(), 2);
    }
}
