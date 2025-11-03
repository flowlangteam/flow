use clap::{Parser, Subcommand};
use colored::Colorize;
use console::style;
use flow_codegen::Compiler;
use flow_parser::Parser as FlowParser;
use flow_transpiler::Transpiler;
use flow_transpiler_java::JavaTranspiler;
use indicatif::{ProgressBar, ProgressStyle};
use std::fs;
use std::path::PathBuf;
use std::time::Instant;

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

    // Compiling
    let spinner = create_spinner("Compiling...", quiet);
    let mut compiler = Compiler::new();
    let code_ptr = compiler.compile(&program).map_err(|e| {
        spinner.finish_and_clear();
        format!("Compilation error: {}", e)
    })?;
    spinner.finish_and_clear();

    if !quiet {
        println!("  {} Compiled successfully", "✓".green().bold());
    }

    // Executing
    if !quiet {
        println!("\n{} Executing program...\n", "→".cyan().bold());
        println!("{}", "─".repeat(50).dimmed());
    }

    let exec_start = Instant::now();
    let main_fn: fn() -> i64 = unsafe { std::mem::transmute(code_ptr) };
    let result = main_fn();
    let exec_time = exec_start.elapsed();

    if !quiet {
        println!("{}", "─".repeat(50).dimmed());
        println!(
            "\n{} Program exited with code: {}",
            "✓".green().bold(),
            style(result).cyan().bold()
        );
    }

    let total_time = start_time.elapsed();

    if show_stats || verbose {
        println!("\n{}", style("Statistics:").bold().underlined());
        println!("  Total time:     {:?}", total_time);
        println!("  Execution time: {:?}", exec_time);
        println!("  Source size:    {} bytes", source.len());
        println!("  AST items:      {}", program.items.len());
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

    // Compiling
    let spinner = create_spinner("Compiling...", quiet);
    let mut compiler = Compiler::new();
    compiler.compile(&program).map_err(|e| {
        spinner.finish_and_clear();
        format!("Compilation error: {}", e)
    })?;
    spinner.finish_and_clear();

    if !quiet {
        println!("  {} Compiled successfully", "✓".green().bold());
    }

    let output_name = output.unwrap_or_else(|| {
        path.file_stem()
            .unwrap()
            .to_string_lossy()
            .into_owned()
            .into()
    });

    if !quiet {
        println!(
            "  {} Note: JIT compilation complete",
            "ℹ".blue()
        );
        println!(
            "  {} Native executable generation not yet implemented",
            "ℹ".blue()
        );
        println!(
            "  {} Would output to: {}",
            "→".cyan(),
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
            return Err("Python transpiler not yet implemented. Coming soon!".to_string());
        }
        "javascript" | "js" => {
            spinner.finish_and_clear();
            return Err("JavaScript transpiler not yet implemented. Coming soon!".to_string());
        }
        "c" => {
            spinner.finish_and_clear();
            return Err("C transpiler not yet implemented. Coming soon!".to_string());
        }
        "rust" => {
            spinner.finish_and_clear();
            return Err("Rust transpiler not yet implemented. Coming soon!".to_string());
        }
        "wasm" | "webassembly" => {
            spinner.finish_and_clear();
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

                let mut compiler = Compiler::new();
                match compiler.compile(&program) {
                    Ok(code_ptr) => {
                        let main_fn: fn() -> i64 = unsafe { std::mem::transmute(code_ptr) };
                        let result = main_fn();
                        println!("{} {}", "=>".green().bold(), style(result).cyan());
                    }
                    Err(e) => eprintln!("{} {}", "✗".red().bold(), e.red()),
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
