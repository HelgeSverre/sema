use sea_orm::DatabaseConnection;
use sqlx::sqlite::SqlitePoolOptions;

pub type Db = DatabaseConnection;

pub async fn connect(database_url: &str) -> Db {
    // Run migrations with a temporary sqlx pool (SeaORM doesn't support sqlx::migrate! directly)
    let sqlx_pool = SqlitePoolOptions::new()
        .max_connections(1)
        .connect(database_url)
        .await
        .expect("Failed to connect for migrations");

    sqlx::query("PRAGMA journal_mode=WAL")
        .execute(&sqlx_pool)
        .await
        .expect("Failed to set WAL mode");

    sqlx::migrate!("./migrations")
        .run(&sqlx_pool)
        .await
        .expect("Failed to run migrations");

    drop(sqlx_pool);

    // Connect with SeaORM for all query operations
    let mut opts = sea_orm::ConnectOptions::new(database_url.to_string());
    opts.max_connections(5);

    sea_orm::Database::connect(opts)
        .await
        .expect("Failed to connect to database")
}
