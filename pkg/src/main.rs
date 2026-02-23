use sema_pkg::{build_router, AppState};
use std::sync::Arc;
use tracing_subscriber::EnvFilter;

#[tokio::main]
async fn main() {
    dotenvy::dotenv().ok();
    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .init();

    let config = sema_pkg::config::Config::from_env();

    // Ensure data directories exist before connecting
    std::fs::create_dir_all(&config.blob_dir).expect("Failed to create blob dir");
    std::fs::create_dir_all("data").ok();

    let db = sema_pkg::db::connect(&config.database_url).await;

    let state = Arc::new(AppState { db, config });
    let addr = format!("{}:{}", state.config.host, state.config.port);

    let app = build_router(state);

    let listener = tokio::net::TcpListener::bind(&addr).await.unwrap();
    tracing::info!("sema-pkg listening on {}", addr);
    axum::serve(listener, app).await.unwrap();
}
