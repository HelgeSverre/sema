use sqlx::sqlite::{SqlitePool, SqlitePoolOptions};

pub type Db = SqlitePool;

pub async fn connect(database_url: &str) -> Db {
    let pool = SqlitePoolOptions::new()
        .max_connections(5)
        .connect(database_url)
        .await
        .expect("Failed to connect to database");

    // Enable WAL mode for better concurrent reads
    sqlx::query("PRAGMA journal_mode=WAL")
        .execute(&pool)
        .await
        .expect("Failed to set WAL mode");

    // Run migrations
    sqlx::migrate!("./migrations")
        .run(&pool)
        .await
        .expect("Failed to run migrations");

    pool
}
