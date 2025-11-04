use colored::Colorize;
use std::collections::HashMap;
use std::fmt;

/// A rich error reporter that displays errors with source context,
/// similar to Rust's error reporting
pub struct ErrorReporter {
    /// Cache of file contents for displaying source context
    file_cache: HashMap<String, String>,
}

#[derive(Debug, Clone)]
pub struct RichError {
    pub title: String,
    pub code: Option<String>,
    pub primary_span: ErrorSpan,
    pub secondary_spans: Vec<ErrorSpan>,
    pub suggestions: Vec<Suggestion>,
    pub notes: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct ErrorSpan {
    pub start: usize,
    pub end: usize,
    pub file: Option<String>,
    pub label: Option<String>,
    pub style: SpanStyle,
}

#[derive(Debug, Clone)]
pub enum SpanStyle {
    Primary,
    Secondary,
    Note,
    Help,
}

#[derive(Debug, Clone)]
pub struct Suggestion {
    pub message: String,
    pub replacements: Vec<Replacement>,
}

#[derive(Debug, Clone)]
pub struct Replacement {
    pub span: ErrorSpan,
    pub text: String,
}

impl ErrorReporter {
    pub fn new() -> Self {
        Self {
            file_cache: HashMap::new(),
        }
    }

    /// Load a file into the cache for error reporting
    pub fn load_file(&mut self, file_path: String, content: String) {
        self.file_cache.insert(file_path, content);
    }

    /// Display a rich error with source context and suggestions
    pub fn display_error(&self, error: &RichError) -> String {
        let mut output = String::new();
        
        // Error header
        output.push_str(&format!("{}: {}\n", 
            "error".red().bold(), 
            error.title.bold()
        ));

        if let Some(code) = &error.code {
            output.push_str(&format!("{}: {}\n", 
                "code".blue().bold(), 
                code
            ));
        }

        // Primary span with source context
        if let Some(source_lines) = self.get_source_context(&error.primary_span) {
            output.push_str(&source_lines);
        }

        // Secondary spans
        for span in &error.secondary_spans {
            if let Some(source_lines) = self.get_source_context(span) {
                output.push_str(&source_lines);
            }
        }

        // Suggestions
        for suggestion in &error.suggestions {
            output.push_str(&format!("{}: {}\n", 
                "help".cyan().bold(), 
                suggestion.message
            ));
            
            for replacement in &suggestion.replacements {
                if let Some(suggestion_lines) = self.get_suggestion_context(&replacement.span, &replacement.text) {
                    output.push_str(&suggestion_lines);
                }
            }
        }

        // Notes
        for note in &error.notes {
            output.push_str(&format!("{}: {}\n", 
                "note".blue().bold(), 
                note
            ));
        }

        output
    }

    /// Get source context around an error span
    fn get_source_context(&self, span: &ErrorSpan) -> Option<String> {
        let file_path = span.file.as_ref()?;
        let content = self.file_cache.get(file_path)?;
        
        let (line_num, col_start, col_end, line_content) = self.get_line_info(content, span.start, span.end)?;
        
        let mut output = String::new();
        
        // File location
        output.push_str(&format!("  {} {}:{}:{}\n",
            "-->".blue().bold(),
            file_path,
            line_num,
            col_start + 1
        ));
        
        // Line number padding
        let line_num_width = line_num.to_string().len();
        let padding = " ".repeat(line_num_width);
        
        // Empty line before source
        output.push_str(&format!("   {}{}\n", padding, "|".blue().bold()));
        
        // Source line
        output.push_str(&format!("{} {} {}\n",
            format!("{:width$}", line_num, width = line_num_width).blue().bold(),
            "|".blue().bold(),
            line_content
        ));
        
        // Error indicator
        let indicator_padding = " ".repeat(col_start);
        let indicator_length = if col_end > col_start { col_end - col_start } else { 1 };
        let indicator = match span.style {
            SpanStyle::Primary => "^".repeat(indicator_length).red().bold(),
            SpanStyle::Secondary => "-".repeat(indicator_length).yellow().bold(),
            SpanStyle::Note => "^".repeat(indicator_length).blue().bold(),
            SpanStyle::Help => "^".repeat(indicator_length).cyan().bold(),
        };
        
        output.push_str(&format!("   {}{}{}{}\n",
            padding,
            "|".blue().bold(),
            indicator_padding,
            indicator
        ));
        
        // Label
        if let Some(label) = &span.label {
            output.push_str(&format!("   {}{}{}{}\n",
                padding,
                "|".blue().bold(),
                indicator_padding,
                label.red()
            ));
        }
        
        Some(output)
    }

    /// Get suggestion context showing the proposed replacement
    fn get_suggestion_context(&self, span: &ErrorSpan, replacement_text: &str) -> Option<String> {
        let file_path = span.file.as_ref()?;
        let content = self.file_cache.get(file_path)?;
        
        let (line_num, col_start, col_end, line_content) = self.get_line_info(content, span.start, span.end)?;
        
        let mut output = String::new();
        
        // Line number padding
        let line_num_width = line_num.to_string().len();
        let padding = " ".repeat(line_num_width);
        
        // Show the line with suggested replacement
        let before = &line_content[..col_start];
        let after = &line_content[col_end..];
        let suggested_line = format!("{}{}{}", before, replacement_text, after);
        
        output.push_str(&format!("{} {} {}\n",
            format!("{:width$}", line_num, width = line_num_width).blue().bold(),
            "|".blue().bold(),
            suggested_line
        ));
        
        // Highlight the replacement
        let indicator_padding = " ".repeat(col_start);
        let indicator_length = replacement_text.len();
        let indicator = "~".repeat(indicator_length).green().bold();
        
        output.push_str(&format!("   {}{}{}{}\n",
            padding,
            "|".blue().bold(),
            indicator_padding,
            indicator
        ));
        
        Some(output)
    }

    /// Extract line information from source content
    fn get_line_info(&self, content: &str, start: usize, end: usize) -> Option<(usize, usize, usize, String)> {
        let lines: Vec<&str> = content.lines().collect();
        let mut char_pos = 0;
        
        for (line_idx, line) in lines.iter().enumerate() {
            let line_start = char_pos;
            let line_end = char_pos + line.len();
            
            if start >= line_start && start <= line_end {
                let col_start = start - line_start;
                let col_end = if end <= line_end { end - line_start } else { line.len() };
                return Some((line_idx + 1, col_start, col_end, line.to_string()));
            }
            
            char_pos = line_end + 1; // +1 for newline
        }
        
        None
    }
}

impl Default for ErrorReporter {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for RichError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let reporter = ErrorReporter::new();
        write!(f, "{}", reporter.display_error(self))
    }
}