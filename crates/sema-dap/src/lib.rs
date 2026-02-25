pub mod protocol;
pub mod server;
pub mod transport;

pub async fn run_server() {
    eprintln!("Sema DAP server starting on stdio...");
    server::run().await;
}
