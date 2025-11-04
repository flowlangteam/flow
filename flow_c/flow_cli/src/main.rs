use clap::{Parser, Subcommand};
use colored::Colorize;
use console::style;
use flow_codegen::{Compiler, AOTCompiler};
use flow_parser::Parser as FlowParser;
use flow_transpiler::Transpiler;
use flow_transpiler_java::JavaTranspiler;
use indicatif::{ProgressBar, ProgressStyle};
use std::fs;
use std::path::PathBuf;
use std::time::Instant;

mod module_manager;
mod error_reporter;

use module_manager::ModuleManager;
use error_reporter::{ErrorReporter, RichError, ErrorSpan, SpanStyle, Suggestion};

const VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(Parser)]
#[command(name = "flow")]
#[command(version = VERSION)]
#[command(about = "Flow Programming Language Compiler", long_about = None)]
#[command(author = "Flow Team")]
struct Cli {
    #[command(subcommand)]
    command: Commands,

    /// Enable verbose output
    #[arg(short, long, global = true)]
    verbose: bool,

    /// Suppress all output except errors
    #[arg(short, long, global = true)]
    quiet: bool,
}

#[derive(Subcommand)]
enum Commands {
    /// Compile and run a Flow program
    Run {
        /// Path to the Flow source file
        file: PathBuf,

        /// Show compilation statistics
        #[arg(short, long)]
        stats: bool,
    },
    /// Compile a Flow program to native code
    Build {
        /// Path to the Flow source file
        file: PathBuf,

        /// Output file path
        #[arg(short, long)]
        output: Option<PathBuf>,

        /// Enable release optimizations
        #[arg(short, long)]
        release: bool,

        /// Show compilation statistics
        #[arg(short, long)]
        stats: bool,
    },
    /// Transpile Flow code to another language
    Transpile {
        /// Path to the Flow source file
        file: PathBuf,

        /// Target language (java, python, javascript, etc.)
        #[arg(short, long, default_value = "java")]
        target: String,

        /// Output file path
        #[arg(short, long)]
        output: Option<PathBuf>,

        /// Class name for Java/JVM targets
        #[arg(short, long)]
        class: Option<String>,

        /// Show compilation statistics
        #[arg(short, long)]
        stats: bool,
    },
    /// Check a Flow program for errors without compiling
    Check {
        /// Path to the Flow source file
        file: PathBuf,

        /// Show detailed AST information
        #[arg(short, long)]
        ast: bool,
    },
    /// Start an interactive REPL session
    Repl {
        /// Show AST for each expression
        #[arg(short, long)]
        ast: bool,
    },
}

fn main() {
    let cli = Cli::parse();

    let result = match cli.command {
        Commands::Run { file, stats } => run_file(&file, stats, cli.verbose, cli.quiet),
        Commands::Build {
            file,
            output,
            release,
            stats,
        } => build_file(&file, output, release, stats, cli.verbose, cli.quiet),
        Commands::Transpile {
            file,
            target,
            output,
            class,
            stats,
        } => transpile_file(&file, &target, output, class, stats, cli.verbose, cli.quiet),
        Commands::Check { file, ast } => check_file(&file, ast, cli.verbose, cli.quiet),
        Commands::Repl { ast } => start_repl(ast, cli.verbose, cli.quiet),
    };

    if let Err(e) = result {
        eprintln!("{} {}", "✗".red().bold(), e.red());
        std::process::exit(1);
    }
}

fn run_file(
    path: &PathBuf,
    show_stats: bool,
    verbose: bool,
    quiet: bool,
) -> Result<(), String> {
    let start_time = Instant::now();

    if !quiet {
        println!(
            "{} Running {}",
            "→".cyan().bold(),
            style(path.display()).yellow()
        );
    }

    // File reading
    let source = fs::read_to_string(path).map_err(|e| format!("Failed to read file: {}", e))?;

    if verbose {
        println!(
            "  {} Read {} bytes",
            "ℹ".blue(),
            style(source.len()).cyan()
        );
    }

    // Parsing with rich error reporting
    let spinner = create_spinner("Parsing...", quiet);
    let mut parser = FlowParser::new(&source);
    let program = match parser.parse() {
        Ok(program) => {
            spinner.finish_and_clear();
            program
        }
        Err(e) => {
            spinner.finish_and_clear();
            
            // Use rich error reporting for parse errors
            let rich_error = parse_error_to_rich(&e, &path.to_string_lossy(), &source);
            display_rich_errors(&[rich_error], &path.to_string_lossy(), &source);
            
            return Err("Parse failed".to_string());
        }
    };

    if !quiet {
        println!(
            "  {} Parsed {} items",
            "✓".green().bold(),
            style(program.items.len()).cyan()
        );
    }

    // Analyzing and compiling
    let spinner = create_spinner("Analyzing dependencies...", quiet);
    let mut module_manager = ModuleManager::new();
    
    // Set source context for better error spans
    module_manager.set_source_context(source.clone(), path.to_string_lossy().to_string());
    
    // Add the directory containing the input file as a search path
    if let Some(parent_dir) = path.parent() {
        module_manager.add_module_search_path(parent_dir.to_path_buf());
    }
    
    let code_ptr = match module_manager.compile_program(&program) {
        Ok(ptr) => {
            spinner.finish_and_clear();
            ptr
        }
        Err((error_msg, analysis_errors)) => {
            spinner.finish_and_clear();
            
            if let Some(errors) = analysis_errors {
                // Use rich error reporting for analysis errors
                let rich_errors = analysis_errors_to_rich(&errors, &path.to_string_lossy());
                display_rich_errors(&rich_errors, &path.to_string_lossy(), &source);
            } else {
                eprintln!("Compilation error: {}", error_msg);
            }
            
            return Err("Compilation failed".to_string());
        }
    };

    // Show any warnings
    let warnings = module_manager.get_warnings();
    if !warnings.is_empty() && !quiet {
        for warning in warnings {
            println!("  {} {:?}", "⚠".yellow().bold(), warning);
        }
    }

    if !quiet {
        println!("  {} Compiled successfully", "✓".green().bold());
        println!("\n{} Executing program...", "→".cyan().bold());
        println!("\n{}", "─".repeat(70));
    }

    // Execute the main function
    type MainFunc = extern "C" fn() -> i64;
    let main_func: MainFunc = unsafe { std::mem::transmute(code_ptr) };
    let result = main_func();

    if !quiet {
        println!("{}", "─".repeat(70));
        println!(
            "\n{} Program exited with code: {}",
            "✓".green().bold(),
            style(result).cyan()
        );

        if show_stats {
            let elapsed = start_time.elapsed();
            println!(
                "  {} Total time: {:.2}ms",
                "ℹ".blue(),
                elapsed.as_secs_f64() * 1000.0
            );
        }
    }

    Ok(())
}

fn build_file(
    path: &PathBuf,
    output: Option<PathBuf>,
    release: bool,
    show_stats: bool,
    verbose: bool,
    quiet: bool,
) -> Result<(), String> {
    let start_time = Instant::now();

    if !quiet {
        println!(
            "{} Building {}",
            "→".cyan().bold(),
            style(path.display()).yellow()
        );
        if release {
            println!("  {} Release mode enabled", "ℹ".blue());
        }
    }

    // Reading file
    let spinner = create_spinner("Reading source file...", quiet);
    let source =
        fs::read_to_string(path).map_err(|e| format!("Failed to read file: {}", e))?;
    spinner.finish_and_clear();

    if verbose {
        println!(
            "  {} Read {} bytes",
            "ℹ".blue(),
            style(source.len()).cyan()
        );
    }

    // Parsing
    let spinner = create_spinner("Parsing...", quiet);
    let mut parser = FlowParser::new(&source);
    let program = parser.parse().map_err(|e| {
        spinner.finish_and_clear();
        format!(
            "Parse error at {}..{}: {}",
            e.span.start, e.span.end, e.message
        )
    })?;
    spinner.finish_and_clear();

    if !quiet {
        println!(
            "  {} Parsed {} items",
            "✓".green().bold(),
            style(program.items.len()).cyan()
        );
    }

    // Analyzing and compiling with module support
    let spinner = create_spinner("Analyzing dependencies...", quiet);
    let mut module_manager = ModuleManager::new();
    
    // Add the directory containing the input file as a search path
    if let Some(parent_dir) = path.parent() {
        module_manager.add_module_search_path(parent_dir.to_path_buf());
    }
    
    module_manager.compile_program(&program).map_err(|(error_msg, _)| {
        spinner.finish_and_clear();
        format!("Compilation error: {}", error_msg)
    })?;
    spinner.finish_and_clear();

    if !quiet {
        println!("  {} Compiled successfully", "✓".green().bold());
    }

    // AOT compilation
    let spinner = create_spinner("Generating native code...", quiet);
    let aot_compiler = AOTCompiler::new();
    let object_code = aot_compiler.compile_to_object(&program).map_err(|e| {
        spinner.finish_and_clear();
        format!("AOT compilation error: {}", e)
    })?;
    spinner.finish_and_clear();

    if !quiet {
        println!("  {} Generated object code", "✓".green().bold());
    }

    // Linking
    let output_name = output.unwrap_or_else(|| {
        path.file_stem()
            .unwrap()
            .to_string_lossy()
            .into_owned()
            .into()
    });

    let spinner = create_spinner("Linking executable...", quiet);
    AOTCompiler::generate_executable(&object_code, &output_name.to_string_lossy()).map_err(|e| {
        spinner.finish_and_clear();
        format!("Linking error: {}", e)
    })?;
    spinner.finish_and_clear();

    if !quiet {
        println!(
            "  {} Executable created: {}",
            "✓".green().bold(),
            style(output_name.display()).yellow()
        );
    }

    let total_time = start_time.elapsed();

    if show_stats || verbose {
        println!("\n{}", style("Statistics:").bold().underlined());
        println!("  Total time:  {:?}", total_time);
        println!("  Source size: {} bytes", source.len());
        println!("  AST items:   {}", program.items.len());
    }

    Ok(())
}

fn transpile_file(
    path: &PathBuf,
    target: &str,
    output: Option<PathBuf>,
    class_name: Option<String>,
    show_stats: bool,
    verbose: bool,
    quiet: bool,
) -> Result<(), String> {
    let start_time = Instant::now();

    if !quiet {
        println!(
            "{} Transpiling {} to {}",
            "→".cyan().bold(),
            style(path.display()).yellow(),
            style(target.to_uppercase()).green()
        );
    }

    // Reading file
    let spinner = create_spinner("Reading source file...", quiet);
    let source = fs::read_to_string(path).map_err(|e| format!("Failed to read file: {}", e))?;
    spinner.finish_and_clear();

    // Parsing
    let spinner = create_spinner("Parsing...", quiet);
    let mut parser = FlowParser::new(&source);
    let program = parser.parse().map_err(|e| {
        spinner.finish_and_clear();
        format!("Parse error: {:?}", e)
    })?;
    spinner.finish_and_clear();

    if !quiet {
        println!("  {} Parsed successfully", "✓".green().bold());
    }

    // Transpiling
    let spinner = create_spinner(&format!("Transpiling to {}...", target.to_uppercase()), quiet);
    
    let output_bytes = match target.to_lowercase().as_str() {
        "java" | "jvm" | "bytecode" => {
            let class_name = class_name.unwrap_or_else(|| {
                path.file_stem()
                    .unwrap()
                    .to_string_lossy()
                    .chars()
                    .next()
                    .map(|c| c.to_uppercase().to_string())
                    .unwrap_or_else(|| "Main".to_string())
                    + &path.file_stem()
                        .unwrap()
                        .to_string_lossy()[1..]
            });
            
            let mut transpiler = JavaTranspiler::new(&class_name);
            transpiler.transpile(&program).map_err(|e| {
                spinner.finish_and_clear();
                format!("Transpilation error: {}", e)
            })?
        }
        "python" | "py" => {
            spinner.finish_and_clear();
            // @TODO: Implement Python transpiler backend
            return Err("Python transpiler not yet implemented. Coming soon!".to_string());
        }
        "javascript" | "js" => {
            spinner.finish_and_clear();
            // @TODO: Implement JavaScript transpiler backend
            return Err("JavaScript transpiler not yet implemented. Coming soon!".to_string());
        }
        "c" => {
            spinner.finish_and_clear();
            // @TODO: Implement C transpiler backend
            return Err("C transpiler not yet implemented. Coming soon!".to_string());
        }
        "rust" => {
            spinner.finish_and_clear();
            // @TODO: Implement Rust transpiler backend
            return Err("Rust transpiler not yet implemented. Coming soon!".to_string());
        }
        "wasm" | "webassembly" => {
            spinner.finish_and_clear();
            // @TODO: Implement WebAssembly transpiler backend
            return Err("WebAssembly transpiler not yet implemented. Coming soon!".to_string());
        }
        _ => {
            spinner.finish_and_clear();
            return Err(format!("Unknown target language: {}. Supported targets: java, python, javascript, c, rust, wasm", target));
        }
    };
    
    spinner.finish_and_clear();

    if !quiet {
        println!("  {} Transpiled successfully", "✓".green().bold());
    }

    // Writing output
    let output_path = output.unwrap_or_else(|| {
        let base = path.file_stem().unwrap().to_string_lossy();
        let extension = match target.to_lowercase().as_str() {
            "java" | "jvm" | "bytecode" => "class",
            "python" | "py" => "py",
            "javascript" | "js" => "js",
            "c" => "c",
            "rust" => "rs",
            "wasm" | "webassembly" => "wasm",
            _ => "out",
        };
        PathBuf::from(format!("{}.{}", base, extension))
    });

    let spinner = create_spinner("Writing output...", quiet);
    fs::write(&output_path, output_bytes)
        .map_err(|e| format!("Failed to write output: {}", e))?;
    spinner.finish_and_clear();

    if !quiet {
        println!(
            "\n{} Transpilation complete: {}",
            "✓".green().bold(),
            style(output_path.display()).cyan()
        );
    }

    let total_time = start_time.elapsed();

    if show_stats || verbose {
        println!("\n{}", style("Statistics:").bold().underlined());
        println!("  Total time:   {:?}", total_time);
        println!("  Source size:  {} bytes", source.len());
        println!("  Output size:  {} bytes", fs::metadata(&output_path).unwrap().len());
        println!("  AST items:    {}", program.items.len());
        println!("  Target:       {}", target.to_uppercase());
    }

    Ok(())
}

fn check_file(
    path: &PathBuf,
    show_ast: bool,
    verbose: bool,
    quiet: bool,
) -> Result<(), String> {
    if !quiet {
        println!(
            "{} Checking {}",
            "→".cyan().bold(),
            style(path.display()).yellow()
        );
    }

    let spinner = create_spinner("Reading source file...", quiet);
    let source =
        fs::read_to_string(path).map_err(|e| format!("Failed to read file: {}", e))?;
    spinner.finish_and_clear();

    let spinner = create_spinner("Parsing...", quiet);
    let mut parser = FlowParser::new(&source);
    let program = parser.parse().map_err(|e| {
        spinner.finish_and_clear();
        format!(
            "Parse error at {}..{}: {}",
            e.span.start, e.span.end, e.message
        )
    })?;
    spinner.finish_and_clear();

    if !quiet {
        println!("  {} No errors found", "✓".green().bold());
        println!(
            "  {} {} item(s) parsed",
            "ℹ".blue(),
            style(program.items.len()).cyan()
        );
    }

    if show_ast || verbose {
        println!("\n{}", style("AST:").bold().underlined());
        println!("{:#?}", program);
    }

    Ok(())
}

fn start_repl(
    show_ast: bool,
    _verbose: bool,
    quiet: bool,
) -> Result<(), String> {
    if !quiet {
        println!(
            "{} {}",
            "Flow REPL".bright_cyan().bold(),
            format!("v{}", VERSION).dimmed()
        );
        println!(
            "  {} Type {} or {} to quit\n",
            "→".cyan(),
            style("exit").yellow(),
            style("quit").yellow()
        );
    }

    use std::io::{self, Write};

    loop {
        print!("{} ", "flow>".bright_cyan().bold());
        io::stdout().flush().unwrap();

        let mut input = String::new();
        io::stdin()
            .read_line(&mut input)
            .map_err(|e| format!("Failed to read input: {}", e))?;

        let input = input.trim();
        if input == "exit" || input == "quit" {
            break;
        }

        if input.is_empty() {
            continue;
        }

        // Wrap in a main function for execution
        let wrapped = format!("fn main() -> Int {{ {} }}", input);

        let mut parser = FlowParser::new(&wrapped);
        match parser.parse() {
            Ok(program) => {
                if show_ast {
                    println!("{}", "AST:".dimmed());
                    println!("{:#?}", program);
                }

                let mut module_manager = ModuleManager::new();
                match module_manager.compile_program(&program) {
                    Ok(code_ptr) => {
                        let main_fn: fn() -> i64 = unsafe { std::mem::transmute(code_ptr) };
                        let result = main_fn();
                        println!("{} {}", "=>".green().bold(), style(result).cyan());
                    }
                    Err((error_msg, _)) => eprintln!("{} {}", "✗".red().bold(), error_msg.red()),
                }
            }
            Err(e) => eprintln!("{} {}", "✗".red().bold(), e.message.red()),
        }
    }

    if !quiet {
        println!("\n{}", "Goodbye!".dimmed());
    }
    Ok(())
}

fn create_spinner(msg: &str, quiet: bool) -> ProgressBar {
    if quiet {
        ProgressBar::hidden()
    } else {
        let spinner = ProgressBar::new_spinner();
        spinner.set_style(
            ProgressStyle::default_spinner()
                .tick_strings(&["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"])
                .template("{spinner:.cyan} {msg}")
                .unwrap(),
        );
        spinner.set_message(msg.to_string());
        spinner.enable_steady_tick(std::time::Duration::from_millis(80));
        spinner
    }
}

/// Convert a parse error to a rich error with suggestions
fn parse_error_to_rich(error: &flow_parser::ParseError, file_path: &str, source: &str) -> RichError {
    let mut suggestions = Vec::new();
    
    // Add specific suggestions based on the error message
    if error.message.contains("Expected identifier") {
        suggestions.push(Suggestion {
            message: "identifiers must start with a letter or underscore, followed by letters, digits, or underscores".to_string(),
            replacements: vec![],
        });
    } else if error.message.contains("Expected type") {
        suggestions.push(Suggestion {
            message: "try using a basic type like 'i64', 'f64', 'bool', or 'string'".to_string(),
            replacements: vec![],
        });
    } else if error.message.contains("Expected ';'") {
        suggestions.push(Suggestion {
            message: "add a semicolon at the end of the statement".to_string(),
            replacements: vec![],
        });
    }
    
    RichError {
        title: error.message.clone(),
        code: Some("E0001".to_string()),
        primary_span: ErrorSpan {
            start: error.span.start,
            end: error.span.end,
            file: Some(file_path.to_string()),
            label: Some("unexpected token".to_string()),
            style: SpanStyle::Primary,
        },
        secondary_spans: vec![],
        suggestions,
        notes: vec![],
    }
}

/// Convert analysis errors to rich errors with suggestions
fn analysis_errors_to_rich(errors: &[flow_analyzer::AnalysisError], file_path: &str) -> Vec<RichError> {
    errors.iter().map(|error| {
        let mut suggestions = Vec::new();
        let mut notes = Vec::new();
        
        // Add specific suggestions based on the error message
        if error.message.contains("Undefined function") {
            if let Some(func_name) = extract_function_name(&error.message) {
                suggestions.push(Suggestion {
                    message: format!("make sure '{}' is defined or imported", func_name),
                    replacements: vec![],
                });
                notes.push("functions must be declared before use, or imported from a module".to_string());
            }
        } else if error.message.contains("returns") && error.message.contains("but expected") {
            suggestions.push(Suggestion {
                message: "make sure the return type matches the function signature".to_string(),
                replacements: vec![],
            });
        } else if error.message.contains("Module file not found") {
            suggestions.push(Suggestion {
                message: "check the module path and ensure the file exists".to_string(),
                replacements: vec![],
            });
            notes.push("modules should be in the same directory or a subdirectory".to_string());
        }
        
        let code = match error.severity {
            flow_analyzer::Severity::Error => "E0002",
            flow_analyzer::Severity::Warning => "W0001",
            _ => "I0001",
        };
        
        RichError {
            title: error.message.clone(),
            code: Some(code.to_string()),
            primary_span: ErrorSpan {
                start: error.span.start,
                end: error.span.end,
                file: error.span.file.clone().or_else(|| Some(file_path.to_string())),
                label: Some("error occurred here".to_string()),
                style: SpanStyle::Primary,
            },
            secondary_spans: vec![],
            suggestions,
            notes,
        }
    }).collect()
}

/// Extract function name from error message
fn extract_function_name(message: &str) -> Option<String> {
    if let Some(start) = message.find('\'') {
        if let Some(end) = message[start + 1..].find('\'') {
            return Some(message[start + 1..start + 1 + end].to_string());
        }
    }
    None
}

/// Display rich errors using the error reporter
fn display_rich_errors(errors: &[RichError], file_path: &str, source: &str) {
    let mut reporter = ErrorReporter::new();
    reporter.load_file(file_path.to_string(), source.to_string());
    
    for error in errors {
        print!("{}", reporter.display_error(error));
    }
}