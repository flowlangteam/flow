use crate::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticSeverity {
    Error,
    Warning,
    Note,
    Help,
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

    pub fn with_code(mut self, code: impl Into<String>) -> Self {
        self.code = Some(code.into());
        self
    }

    pub fn with_label(mut self, label: Label) -> Self {
        self.labels.push(label);
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
}
