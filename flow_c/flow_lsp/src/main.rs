use flow_lsp::FlowLanguageServer;
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

#[tokio::main]
async fn main() {
    // Initialize logging
    tracing_subscriber::registry()
        .with(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "flow_lsp=info".into()),
        )
        .with(tracing_subscriber::fmt::layer())
        .init();

    // Start the language server
    FlowLanguageServer::run().await;
}